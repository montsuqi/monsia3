/* -*- Mode: C; c-basic-offset: 4 -*-
 * libglade - a library for building interfaces from XML files at runtime
 * Copyright (C) 1998-2002  James Henstridge <james@daa.com.au>
 *
 * glade-parser.c: functions for parsing glade-2.0 files
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the 
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA  02111-1307, USA.
 */
#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <glib.h>
#include <glib/gi18n-lib.h>
#include <libxml/parser.h>
#include <time.h>

#include "glade-parser.h"
#include "glade.h"

#define GLADE_NOTE(a,b)

typedef enum {
    PARSER_START,
    PARSER_GTK_INTERFACE,
    PARSER_WIDGET,
    PARSER_WIDGET_ATTR,
	PARSER_SIGNAL,
	PARSER_SIGNAL_ATTR,
	PARSER_ACCEL,
	PARSER_ACCEL_ATTR,
	PARSER_CHILD,
	PARSER_CHILD_ATTR,
    PARSER_FINISH,
    PARSER_UNKNOWN
} ParserState;

static const gchar *state_names[] = {
    "PARSER_START",
    "PARSER_GTK_INTERFACE",
    "PARSER_WIDGET",
    "PARSER_WIDGET_ATTR",
	"PARSER_SIGNAL",
	"PARSER_SIGNAL_ATTR",
	"PARSER_ACCEL",
	"PARSER_ACCEL_ATTR",
	"PARSER_CHILD",
	"PARSER_CHILD_ATTR",
    "PARSER_FINISH",
    "PARSER_UNKNOWN"
};

typedef struct _GladeParseState GladeParseState;
struct _GladeParseState {
    ParserState state;

    const gchar *domain;

    guint unknown_depth;    /* handle recursive unrecognised tags */
    ParserState prev_state; /* the last `known' state we were in */

    guint widget_depth;
    GString *content;

    GladeInterface *interface;
    GladeWidgetInfo *widget;

    GArray *signals;
	GladeSignalInfo *signal_info;
    GArray *accels;
	GladeAccelInfo *accel_info;
	
	GHashTable *group_table;
};


static inline void
flush_properties(GladeParseState *state)
{
	GladeWidgetInfo *parent = state->widget->parent;
	GladeAttribute *attr;
	GladePropInfo prop;
	GladeChildInfo *child_info;
	GArray *props = g_array_new(FALSE,FALSE,sizeof(GladePropInfo));
	GArray *child_props = g_array_new(FALSE,FALSE,sizeof(GladePropInfo));
	int i;

    if (!state->widget->attrs) {
		return;
	}

	prop.has_context = 1;
	prop.translatable = 1;
	prop.comment = "";
	for (i = 0; i < g_list_length(state->widget->attrs); i++) {
		attr = (GladeAttribute*)g_list_nth_data(state->widget->attrs,i);
		if (
			!strcmp(attr->name,"left_attach") ||
			!strcmp(attr->name,"right_attach") ||
			!strcmp(attr->name,"top_attach") ||
			!strcmp(attr->name,"bottom_attach")
		) {
			prop.name = attr->name;
			prop.value = attr->value;
			g_array_append_val(child_props, prop);
		} else if (
                   !strcmp(attr->name,"x") ||
                   !strcmp(attr->name,"y") ) {
            if (!strcmp(state->widget->classname,"GtkPandaWindow")) {
			  prop.name = attr->name;
			  prop.value = attr->value;
			  g_array_append_val(props, prop);
            } else {
			  prop.name = attr->name;
			  prop.value = attr->value;
			  g_array_append_val(child_props, prop);
            }
		} else if (!strcmp(attr->name,"width")) {
			prop.name = strdup("width_request");
			prop.value = attr->value;
			g_array_append_val(props, prop);
		} else if (!strcmp(attr->name,"height")) {
			prop.name = strdup("height_request");
			prop.value = attr->value;
			g_array_append_val(props, prop);
		} else if (!strcmp(attr->name,"input_mode")) {
            gchar *buf = g_malloc0(64);
			prop.name = attr->name;
            sprintf(buf,"GTK_PANDA_ENTRY_%s",attr->value);
			prop.value = buf;
			g_array_append_val(props, prop);
		} else if (!strcmp(attr->name,"type")) {
			prop.name = attr->name;
			if (!strcmp(attr->value,"GTK_WINDOW_DIALOG")) {
				prop.value = "popup";
			} else {
				prop.value = "toplevel";
			}
			g_array_append_val(props, prop);
		} else if (!strcmp(attr->name,"position")) {
			prop.name = "window_position";
			if (!strcmp(attr->value,"GTK_WIN_POS_CENTER")) {
				prop.value = "center";
			} else {
				prop.value = "none";
			}
			g_array_append_val(props, prop);
		} else if (!strcmp(attr->name,"child_name")) {
			if (!strcmp(attr->value,"Notebook:tab")) {
			  prop.name = "type";
			  prop.value = "tab";
			  g_array_append_val(child_props, prop);
			  prop.name = "tabl_fill";
			  prop.value = "False";
			  g_array_append_val(child_props, prop);
			}
		} else if (!strcmp(state->widget->classname,"GtkRadioButton") &&
                   !strcmp(attr->name,"group")) {
			gchar *groupname = (gchar*)g_hash_table_lookup(state->group_table,attr->value);
			if (groupname != NULL) {
				prop.value = groupname;
			} else {
				prop.value = state->widget->name;
				g_hash_table_insert(state->group_table,attr->value,state->widget->name);
			}
			prop.name = attr->name;
			g_array_append_val(props, prop);
		} else {
			prop.name = attr->name;
			prop.value = attr->value;
			g_array_append_val(props, prop);
		}
	}
	state->widget->properties = (GladePropInfo*)props->data;
	state->widget->n_properties = props->len;
	g_array_free(props, FALSE);

	if (parent != NULL) {
		for(i = 0; i < parent->n_children; i++) {
			child_info = &parent->children[i];
			if (child_info->child == state->widget) {
				child_info->properties = (GladePropInfo*)child_props->data;
				child_info->n_properties = child_props->len;
			}
		}
	}
	state->widget->attrs = NULL;
	g_array_free(child_props, FALSE);
}

static inline void
flush_signals(GladeParseState *state)
{
    if (state->signals) {
	state->widget->signals = (GladeSignalInfo *)state->signals->data;
	state->widget->n_signals = state->signals->len;
	g_array_free(state->signals, FALSE);
    }
    state->signals = NULL;
}

static inline void
flush_accels(GladeParseState *state)
{
    if (state->accels) {
	state->widget->accels = (GladeAccelInfo *)state->accels->data;
	state->widget->n_accels = state->accels->len;
	g_array_free(state->accels, FALSE);
    }
    state->accels = NULL;
}

static void
glade_parser_start_document(GladeParseState *state)
{
    state->state = PARSER_START;

    state->unknown_depth = 0;
    state->prev_state = PARSER_UNKNOWN;

    state->widget_depth = 0;
    state->content = g_string_sized_new(128);

    state->interface = glade_parser_interface_new ();
    state->widget = NULL;
    state->signals = NULL;
    state->accels = NULL;
}

static void
glade_parser_end_document(GladeParseState *state)
{
    g_string_free(state->content, TRUE);

    if (state->unknown_depth != 0)
	g_warning("unknown_depth != 0 (%d)", state->unknown_depth);
    if (state->widget_depth != 0)
	g_warning("widget_depth != 0 (%d)", state->widget_depth);
}

static void
glade_parser_comment (GladeParseState *state, const xmlChar *comment)
{
	if (state->state == PARSER_START)
		state->interface->comment = g_strdup (CAST_BAD (comment));
}

static void
glade_parser_start_element(GladeParseState *state,
			   const xmlChar *name, const xmlChar **attrs)
{
    GLADE_NOTE(PARSER, g_message("<%s> in state %s",
				 name, state_names[state->state]));

    switch (state->state) {
    case PARSER_START:
		if (!xmlStrcmp(name, BAD_CAST("GTK-Interface"))) {
		    state->state = PARSER_GTK_INTERFACE;
		} else {
		    g_warning("Expected <GTK-Interface>.  Got <%s>.", name);
		    state->prev_state = state->state;
		    state->state = PARSER_UNKNOWN;
		    state->unknown_depth++;
		}
		break;
    case PARSER_GTK_INTERFACE:
		if (!xmlStrcmp(name, BAD_CAST("widget"))) {
		    GladeInterface *iface = state->interface;

		    iface->n_toplevels++;
		    iface->toplevels = g_renew(GladeWidgetInfo *, iface->toplevels,
					       iface->n_toplevels);
		    state->widget = g_new0(GladeWidgetInfo, 1);
		    iface->toplevels[iface->n_toplevels-1] = state->widget;

		    state->widget_depth++;
		    state->signals = NULL;
			state->signal_info = NULL;
		    state->accels = NULL;
			state->accel_info = NULL;
		    state->state = PARSER_WIDGET;
			state->group_table = g_hash_table_new(g_str_hash,g_str_equal);
		} else {
		    state->prev_state = state->state;
		    state->state = PARSER_UNKNOWN;
		    state->unknown_depth++;
		}
		break;
    case PARSER_WIDGET:
		if (!xmlStrcmp(name, BAD_CAST("signal"))) {
		    state->state = PARSER_SIGNAL;
		} else if (!xmlStrcmp(name, BAD_CAST("accelerator"))) {
		    state->state = PARSER_ACCEL;
		} else if (!xmlStrcmp(name, BAD_CAST("child"))) {
		    state->state = PARSER_CHILD;
		} else if (!xmlStrcmp(name, BAD_CAST("widget"))) {
			GladeWidgetInfo *parent = state->widget;
			GladeChildInfo *child_info;
			// flush 
			flush_properties(state);
			flush_signals(state);
			flush_accels(state);
			// new child widget
		    state->widget = g_new0(GladeWidgetInfo, 1);
			state->widget->parent = parent;

			parent->n_children++;
			parent->children = g_renew(GladeChildInfo, 
				parent->children, parent->n_children);
			child_info = &parent->children[parent->n_children-1];
			child_info->internal_child = NULL;
			child_info->properties = NULL;
			child_info->n_properties = 0;
			child_info->child = state->widget;

		    state->widget_depth++;
		    state->signals = NULL;
			state->signal_info = NULL;
		    state->accels = NULL;
			state->accel_info = NULL;
		    state->state = PARSER_WIDGET;
		} else {
		    state->state = PARSER_WIDGET_ATTR;
		}
		break;
    case PARSER_SIGNAL:
		state->state = PARSER_SIGNAL_ATTR;
		if (!state->signals) {
			state->signals = g_array_new(FALSE,FALSE,sizeof(GladeSignalInfo));
		}
		break;
    case PARSER_ACCEL:
		state->state = PARSER_ACCEL_ATTR;
		if (!state->accels) {
			state->accels = g_array_new(FALSE,FALSE,sizeof(GladeAccelInfo));
		}
		break;
	case PARSER_CHILD:
		state->state = PARSER_CHILD_ATTR;
		break;
	case PARSER_WIDGET_ATTR:
	case PARSER_SIGNAL_ATTR:
	case PARSER_ACCEL_ATTR:
	case PARSER_CHILD_ATTR:
		g_warning("There should be no elements here.  Found <%s>.", name);
		break;
    case PARSER_FINISH:
		g_warning("There should be no elements here.  Found <%s>.", name);
		state->prev_state = state->state;
		state->state = PARSER_UNKNOWN;
		state->unknown_depth++;
		break;
   	case PARSER_UNKNOWN:
		state->unknown_depth++;
		break;
    }
    /* truncate the content string ... */
    g_string_truncate(state->content, 0);
}

static void
glade_parser_end_element(GladeParseState *state, const xmlChar *name)
{
    GLADE_NOTE(PARSER, g_message("</%s> in state %s",
				 name, state_names[state->state]));

    switch (state->state) {
    case PARSER_GTK_INTERFACE:
        state->state = PARSER_FINISH;
        break;
    case PARSER_UNKNOWN:
		state->unknown_depth--;
		if (state->unknown_depth == 0)
		    state->state = state->prev_state;
		break;
	case PARSER_WIDGET_ATTR:
		state->state = PARSER_WIDGET;
		if (!xmlStrcmp(name,BAD_CAST("class"))) {
            if (!strcmp(state->content->str,"GtkPandaPS")) {
			    state->widget->classname = 
			    	glade_xml_alloc_string(state->interface, "GtkPandaPDF");
            } else if (!strcmp(state->content->str,"GnomePixmap")) {
			    state->widget->classname = 
			    	glade_xml_alloc_string(state->interface, "GtkPandaPixmap");
            } else if (!strcmp(state->content->str,"GnomeFileEntry")) {
			    state->widget->classname = 
			    	glade_xml_alloc_string(state->interface, "GtkPandaFileEntry");
            } else if (!strcmp(state->content->str,"GtkWindow")) {
			    state->widget->classname = 
			    	glade_xml_alloc_string(state->interface, "GtkPandaWindow");
            } else {
			    state->widget->classname = 
			    	glade_xml_alloc_string(state->interface, state->content->str);
            }
		} else if (!xmlStrcmp(name,BAD_CAST("name"))) {
			state->widget->name = 
				glade_xml_alloc_string(state->interface, state->content->str);
			g_hash_table_insert(state->interface->names, 
				state->widget->name, state->widget);
		} else {
			GladeAttribute *attr = g_new0(GladeAttribute, 1);
			attr->name = 
				g_strdup(name);
			attr->value = 
				g_strdup(state->content->str);
			state->widget->attrs = 
				g_list_append(state->widget->attrs, attr);
		}
		break;
	case PARSER_CHILD_ATTR:
		state->state = PARSER_CHILD;
		GladeAttribute *attr = g_new0(GladeAttribute, 1);
		if (
			!xmlStrcmp(name, BAD_CAST("left_attach")) ||
			!xmlStrcmp(name, BAD_CAST("right_attach")) ||
			!xmlStrcmp(name, BAD_CAST("top_attach")) ||
			!xmlStrcmp(name, BAD_CAST("bottom_attach"))
		) {
			attr->name = 
				glade_xml_alloc_string(state->interface, name);
			attr->value = 
				glade_xml_alloc_string(state->interface, state->content->str);
			state->widget->attrs = 
				g_list_append(state->widget->attrs, attr);
		}
		break;
    case PARSER_CHILD:
		state->state = PARSER_WIDGET;
		if (xmlStrcmp(name, BAD_CAST("child")) != 0)
	   		g_warning("should find </child> here.  Found </%s>", name);
		break;
    case PARSER_WIDGET:
		if (xmlStrcmp(name, BAD_CAST("widget")) != 0)
	   		g_warning("should find </widget> here.  Found </%s>", name);
		flush_properties(state);
		flush_signals(state);
		flush_accels(state);
		state->widget = state->widget->parent;
		state->widget_depth--;

		if (state->widget_depth == 0)
		    state->state = PARSER_GTK_INTERFACE;
		else
		    state->state = PARSER_WIDGET;
		break;
	case PARSER_SIGNAL_ATTR:
		state->state = PARSER_SIGNAL;
		if (!state->signal_info) {
			state->signal_info = g_new0(GladeSignalInfo, 1);
		}
		if (!xmlStrcmp(name, BAD_CAST("name"))) {
			state->signal_info->name = 
				glade_xml_alloc_string(state->interface, state->content->str);
		} else if (!xmlStrcmp(name, BAD_CAST("handler"))) {
			state->signal_info->handler = 
				glade_xml_alloc_string(state->interface, state->content->str);
		} else if (!xmlStrcmp(name, BAD_CAST("data"))) {
			state->signal_info->object = 
				glade_xml_alloc_string(state->interface, state->content->str);
		} else if (!xmlStrcmp(name, BAD_CAST("object"))) {
			state->signal_info->object = 
				glade_xml_alloc_string(state->interface, state->content->str);
		} else if (!xmlStrcmp(name, BAD_CAST("after"))) {
			state->signal_info->after = state->content->str[0] == 'T';
		}
		break;
	case PARSER_SIGNAL:
		state->state = PARSER_WIDGET;
		if (state->signal_info) {
			g_array_append_val(state->signals, *(state->signal_info));
			state->signal_info = NULL;
		}
		break;
	case PARSER_ACCEL_ATTR:
		state->state = PARSER_ACCEL;
		if (!state->accel_info) {
			state->accel_info = g_new0(GladeAccelInfo, 1);
		}
		if (!xmlStrcmp(name, BAD_CAST("key")) && 
			!strncmp(state->content->str, "GDK_",4)) {
			state->accel_info->key = 
				gdk_keyval_from_name(CAST_BAD(&state->content->str[4]));
		} else if (!xmlStrcmp(name, BAD_CAST("signal"))) {
			state->accel_info->signal = 
				glade_xml_alloc_string(state->interface, state->content->str);
		} else if (!xmlStrcmp(name, BAD_CAST("modifiers"))) {
			const xmlChar *pos   = BAD_CAST(state->content->str);
			const xmlChar *shift = BAD_CAST("GDK_SHIFT_MASK");
			const xmlChar *ctrl  = BAD_CAST("GDK_CONTROL_MASK");
			const xmlChar *mod1  = BAD_CAST("GDK_MOD1_MASK");
			state->accel_info->modifiers = 0;
			while (pos[0]) {
				if (!xmlStrncmp(pos, shift, xmlStrlen(shift))) {
					state->accel_info->modifiers |= GDK_SHIFT_MASK;
					pos += xmlStrlen(shift);
				} else if (!xmlStrncmp(pos, ctrl, xmlStrlen(ctrl))) {
					state->accel_info->modifiers |= GDK_CONTROL_MASK;
					pos += xmlStrlen(ctrl);
				} else if (!xmlStrncmp(pos, mod1, xmlStrlen(mod1))) {
					state->accel_info->modifiers |= GDK_MOD1_MASK;
					pos += xmlStrlen(mod1);
				} else {
					pos++;
				}
			}
		}
		break;
	case PARSER_ACCEL:
		state->state = PARSER_WIDGET;
		if (state->accel_info) {
			g_array_append_val(state->accels, *(state->accel_info));
			state->accel_info = NULL;
		}
		break;
    case PARSER_FINISH:
		g_warning("should not be closing any elements in this state");
		break;
    }
    /* truncate the content string ... */
    g_string_truncate(state->content, 0);
}

static void
glade_parser_characters(GladeParseState *state, const xmlChar *chars, gint len)
{
	g_string_append_len(state->content, CAST_BAD(chars), len);
}

static xmlEntityPtr
glade_parser_get_entity(GladeParseState *state, const xmlChar *name)
{
    return xmlGetPredefinedEntity(name);
}

static void
glade_parser_warning(GladeParseState *state, const gchar *msg, ...)
{
    va_list args;

    va_start(args, msg);
    g_logv("XML", G_LOG_LEVEL_WARNING, msg, args);
    va_end(args);
}

static void
glade_parser_error(GladeParseState *state, const gchar *msg, ...)
{
    va_list args;

    va_start(args, msg);
    g_logv("XML", G_LOG_LEVEL_CRITICAL, msg, args);
    va_end(args);
}

static void
glade_parser_fatal_error(GladeParseState *state, const gchar *msg, ...)
{
    va_list args;

    va_start(args, msg);
    g_logv("XML", G_LOG_LEVEL_ERROR, msg, args);
    va_end(args);
}

static xmlSAXHandler glade_parser = {
    0, /* internalSubset */
    0, /* isStandalone */
    0, /* hasInternalSubset */
    0, /* hasExternalSubset */
    0, /* resolveEntity */
    (getEntitySAXFunc)glade_parser_get_entity, /* getEntity */
    0, /* entityDecl */
    0, /* notationDecl */
    0, /* attributeDecl */
    0, /* elementDecl */
    0, /* unparsedEntityDecl */
    0, /* setDocumentLocator */
    (startDocumentSAXFunc)glade_parser_start_document, /* startDocument */
    (endDocumentSAXFunc)glade_parser_end_document, /* endDocument */
    (startElementSAXFunc)glade_parser_start_element, /* startElement */
    (endElementSAXFunc)glade_parser_end_element, /* endElement */
    0, /* reference */
    (charactersSAXFunc)glade_parser_characters, /* characters */
    0, /* ignorableWhitespace */
    0, /* processingInstruction */
    (commentSAXFunc)glade_parser_comment, /* comment */
    (warningSAXFunc)glade_parser_warning, /* warning */
    (errorSAXFunc)glade_parser_error, /* error */
    (fatalErrorSAXFunc)glade_parser_fatal_error, /* fatalError */
};

static void
widget_info_free(GladeWidgetInfo *info)
{
    gint i;

    if (!info) return;

    g_free(info->properties);
    g_free(info->atk_props);
    g_free(info->signals);
    g_free(info->accels);

    for (i = 0; i < info->n_children; i++) {
	g_free(info->children[i].properties);
	widget_info_free(info->children[i].child);
    }
    g_free(info->children);
    g_free(info);
}

/**
 * glade_parser_interface_new
 *
 * Returns a newly allocated GladeInterface.
 */
GladeInterface *
glade_parser_interface_new ()
{
	GladeInterface *interface;
	interface = g_new0 (GladeInterface, 1);
	interface->names = g_hash_table_new (g_str_hash, g_str_equal);
	interface->strings = g_hash_table_new_full (g_str_hash,
						    g_str_equal,
						    (GDestroyNotify)g_free,
						    NULL);
	return interface;
}

/**
 * glade_parser_interface_destroy
 * @interface: the GladeInterface structure.
 *
 * Frees a GladeInterface structure.
 */
void
glade_parser_interface_destroy (GladeInterface *interface)
{
    gint i;

    g_return_if_fail(interface != NULL);

    /* free requirements */
    g_free(interface->requires);

    for (i = 0; i < interface->n_toplevels; i++)
	widget_info_free(interface->toplevels[i]);
    g_free(interface->toplevels);

    g_hash_table_destroy(interface->names);

    /* free the strings hash table.  The destroy notify will take care
     * of the strings. */
    g_hash_table_destroy(interface->strings);

    g_free (interface->comment);
    
    g_free(interface);
}

gchar* 
to_utf8 (const gchar *str) {
  gsize br, bw;
  GError *err = NULL;
  gchar *tstr = g_convert(str, -1, "utf-8", "euc-jisx0213", &br, &bw, &err);
  if ( err != NULL ) { 
    g_log(G_LOG_DOMAIN, G_LOG_LEVEL_CRITICAL, 
      "convert error(euc-jisx0213 to utf8):%s:%s\n", err->message,str); 
    exit(1);
  }
  return tstr;
}

/**
 * glade_parser_interface_new_from_file
 * @file: the filename of the glade XML file.
 * @domain: the translation domain for the XML file.
 *
 * This function parses a Glade XML interface file to a GladeInterface
 * object (which is libglade's internal representation of the
 * interface data).
 *
 * Generally, user code won't need to call this function.  Instead, it
 * should go through the GladeXML interfaces.
 *
 * Returns: the GladeInterface structure for the XML file.
 */
GladeInterface *
glade_parser_interface_new_from_file (const gchar *file, const gchar *domain)
{
    GladeParseState state = { 0 };
    int prevSubstituteEntities;
    int rc;
    gchar *buf1;
    gchar *buf2;
    gsize size;
    GError *error = NULL;

    if (!g_file_test(file, G_FILE_TEST_IS_REGULAR)) {
	glade_util_ui_message (glade_app_get_window (), 
			       GLADE_UI_ERROR,
			       _("Could not find glade file %s"), file);
	return NULL;
    }

    state.interface = NULL;
    if (domain)
	state.domain = domain;
    else
	state.domain = textdomain(NULL);

    prevSubstituteEntities = xmlSubstituteEntitiesDefault(1);

    g_file_get_contents(file, &buf1, &size, &error);
    if (error != NULL) {
        g_error_free(error);
        return NULL;
    }

    if (strstr(buf1, "encoding=\"EUC-JP\"")) {
      rc = xmlSAXUserParseMemory(&glade_parser, &state, buf1, strlen(buf1));
    } else {
        buf2 = to_utf8(buf1);
        rc = xmlSAXUserParseMemory(&glade_parser, &state, buf2, strlen(buf2));
        g_free(buf2);
    }
    g_free(buf1);

    xmlSubstituteEntitiesDefault(prevSubstituteEntities);

    if (rc < 0) {
	glade_util_ui_message (glade_app_get_window (), 
			       GLADE_UI_ERROR,
			       _("Errors parsing glade file %s"), file);
	if (state.interface)
	    glade_parser_interface_destroy (state.interface);
	return NULL;
    }
    if (state.state != PARSER_FINISH) {
	glade_util_ui_message (glade_app_get_window (), 
			       GLADE_UI_ERROR,
			       _("Errors parsing glade file %s"), file);
	if (state.interface)
	    glade_parser_interface_destroy(state.interface);
	return NULL;
    }
    return state.interface;
}

/**
 * glade_parser_interface_new_from_buffer
 * @buffer: a buffer in memory containing XML data.
 * @len: the length of @buffer.
 * @domain: the translation domain for the XML file.
 *
 * This function is similar to glade_parser_parse_file, except that it
 * parses XML data from a buffer in memory.  This could be used to
 * embed an interface into the executable, for instance.
 *
 * Generally, user code won't need to call this function.  Instead, it
 * should go through the GladeXML interfaces.
 *
 * Returns: the GladeInterface structure for the XML buffer.
 */
GladeInterface *
glade_parser_interface_new_from_buffer (const gchar *buffer,
					gint len,
					const gchar *domain)
{
    GladeParseState state = { 0 };
    int prevSubstituteEntities;
    int rc;

    state.interface = NULL;
    if (domain)
	state.domain = domain;
    else
	state.domain = textdomain(NULL);

    prevSubstituteEntities = xmlSubstituteEntitiesDefault(1);

    rc = xmlSAXUserParseMemory(&glade_parser, &state, buffer, len);

    xmlSubstituteEntitiesDefault(prevSubstituteEntities);

    if (rc < 0) {
	g_warning("document not well formed!");
	if (state.interface)
	    glade_parser_interface_destroy (state.interface);
	return NULL;
    }
    if (state.state != PARSER_FINISH) {
	g_warning("did not finish in PARSER_FINISH state!");
	if (state.interface)
	    glade_parser_interface_destroy(state.interface);
	return NULL;
    }
    return state.interface;
}

static gchar *
modifier_string_from_bits (GdkModifierType modifiers)
{
    GString *string = g_string_new ("");

    if (modifiers & GDK_SHIFT_MASK) {
	if (string->len > 0)
	    g_string_append (string, " | ");
	g_string_append (string, "GDK_SHIFT_MASK");
    }

    if (modifiers & GDK_LOCK_MASK) {
	if (string->len > 0)
	    g_string_append (string, " | ");
	g_string_append (string, "GDK_LOCK_MASK");
    }

    if (modifiers & GDK_CONTROL_MASK) {
	if (string->len > 0)
	    g_string_append (string, " | ");
	g_string_append (string, "GDK_CONTROL_MASK");
    }

    if (modifiers & GDK_MOD1_MASK) {
	if (string->len > 0)
	    g_string_append (string, " | ");
	g_string_append (string, "GDK_MOD1_MASK");
    }

    if (modifiers & GDK_MOD2_MASK) {
	if (string->len > 0)
	    g_string_append (string, " | ");
	g_string_append (string, "GDK_MOD2_MASK");
    }

    if (modifiers & GDK_MOD3_MASK) {
	if (string->len > 0)
	    g_string_append (string, " | ");
	g_string_append (string, "GDK_MOD3_MASK");
    }

    if (modifiers & GDK_MOD4_MASK) {
	if (string->len > 0)
	    g_string_append (string, " | ");
	g_string_append (string, "GDK_MOD4_MASK");
    }

    if (modifiers & GDK_MOD5_MASK) {
	if (string->len > 0)
	    g_string_append (string, " | ");
	g_string_append (string, "GDK_MOD5_MASK");
    }

    if (modifiers & GDK_BUTTON1_MASK) {
	if (string->len > 0)
	    g_string_append (string, " | ");
	g_string_append (string, "GDK_BUTTON1_MASK");
    }

    if (modifiers & GDK_BUTTON2_MASK) {
	if (string->len > 0)
	    g_string_append (string, " | ");
	g_string_append (string, "GDK_BUTTON2_MASK");
    }

    if (modifiers & GDK_BUTTON3_MASK) {
	if (string->len > 0)
	    g_string_append (string, " | ");
	g_string_append (string, "GDK_BUTTON3_MASK");
    }

    if (modifiers & GDK_BUTTON4_MASK) {
	if (string->len > 0)
	    g_string_append (string, " | ");
	g_string_append (string, "GDK_BUTTON4_MASK");
    }

    if (modifiers & GDK_BUTTON5_MASK) {
	if (string->len > 0)
	    g_string_append (string, " | ");
	g_string_append (string, "GDK_BUTTON5_MASK");
    }

    if (modifiers & GDK_RELEASE_MASK) {
	if (string->len > 0)
	    g_string_append (string, " | ");
	g_string_append (string, "GDK_RELEASE_MASK");
    }

    if (string->len > 0)
	return g_string_free (string, FALSE);

    g_string_free (string, TRUE);
    return NULL;
}

/***********************************
 * panda
 ***********************************/

static void
dump_widget_panda(xmlNode *parent_node, GladeWidgetInfo *info, GladeChildInfo *child_info, GladeWidgetInfo *parent)
{
    xmlNode *widget, *node;
    xmlNode *child;
    gint i;
    gboolean f_child_tag = FALSE;

    if (info == NULL) {
	    return;
    }

    widget = xmlNewNode(NULL, BAD_CAST("widget"));
    xmlAddChild(parent_node, widget);

    node = xmlNewNode(NULL, BAD_CAST("class"));
    if (!strcmp(info->classname,"GtkPandaPDF")) {
        xmlNodeAddContent(node, BAD_CAST("GtkPandaPS"));
    } else if (!strcmp(info->classname,"GtkPandaPixmap")) {
        xmlNodeAddContent(node, BAD_CAST("GnomePixmap"));
    } else if (!strcmp(info->classname,"GtkPandaFileEntry")) {
        xmlNodeAddContent(node, BAD_CAST("GnomeFileEntry"));
    } else if (!strcmp(info->classname,"GtkPandaWindow")) {
        xmlNodeAddContent(node, BAD_CAST("GtkWindow"));
    } else {
        xmlNodeAddContent(node, BAD_CAST(info->classname));
    }
    xmlAddChild(widget, node);

    /* child_name */
    if (parent != NULL) {
        if (!strcmp(parent->classname,"GtkPandaCList")) {
            node = xmlNewNode(NULL, BAD_CAST("child_name"));
            xmlNodeAddContent(node, BAD_CAST("CList:title"));
            xmlAddChild(widget, node);
        } else if (!strcmp(parent->classname,"GtkPandaCombo")) {
            node = xmlNewNode(NULL, BAD_CAST("child_name"));
            xmlNodeAddContent(node, BAD_CAST("GtkPandaCombo:entry"));
            xmlAddChild(widget, node);
        } else if (!strcmp(parent->classname,"GtkPandaFileEntry")) {
            node = xmlNewNode(NULL, BAD_CAST("child_name"));
            xmlNodeAddContent(node, BAD_CAST("GnomeEntry:entry"));
            xmlAddChild(widget, node);
        }
    }

    node = xmlNewNode(NULL, BAD_CAST("name"));
    xmlNodeAddContent(node, BAD_CAST(info->name));
    xmlAddChild(widget, node);

    if (child_info != NULL) {
        for (i = 0; i < child_info->n_properties; i++) {
            if (!strcmp(child_info->properties[i].name, "left_attach") ||
                !strcmp(child_info->properties[i].name, "right_attach") ||
                !strcmp(child_info->properties[i].name, "top_attach") ||
                !strcmp(child_info->properties[i].name, "bottom_attach")) {
                f_child_tag = TRUE;
                continue;
            } else if (!strcmp(child_info->properties[i].name, "type")) {
				if (!strcmp(child_info->properties[i].value, "tab")) { 
              		node = xmlNewNode(NULL, BAD_CAST("child_name"));
              		xmlNodeAddContent(node, BAD_CAST("Notebook:tab"));
              		xmlAddChild(widget, node);
				}
            } else {
              node = xmlNewNode(NULL, BAD_CAST(child_info->properties[i].name));
              xmlNodeAddContent(node, BAD_CAST(child_info->properties[i].value));
              xmlAddChild(widget, node);
            }
        }
    }

    /* child */
    if (f_child_tag) {
        node = xmlNewNode(NULL, BAD_CAST("child"));
        xmlAddChild(widget, node);
        for (i = 0; i < child_info->n_properties; i++) {
            if (!strcmp(child_info->properties[i].name, "left_attach") ||
                !strcmp(child_info->properties[i].name, "right_attach") ||
                !strcmp(child_info->properties[i].name, "top_attach") ||
                !strcmp(child_info->properties[i].name, "bottom_attach")) {
                child = xmlNewNode(NULL, BAD_CAST(child_info->properties[i].name));
                xmlNodeAddContent(child, BAD_CAST(child_info->properties[i].value));
                xmlAddChild(node, child);
            }
        }
    }

    for (i = 0; i < info->n_properties; i++) { 
        if (!strcmp(info->properties[i].name,"width_request")) {
            node = xmlNewNode(NULL, BAD_CAST("width"));
            xmlNodeAddContent(node, BAD_CAST(info->properties[i].value));
            xmlAddChild(widget, node);
        } else if (!strcmp(info->properties[i].name,"height_request")) {
            node = xmlNewNode(NULL, BAD_CAST("height"));
            xmlNodeAddContent(node, BAD_CAST(info->properties[i].value));
            xmlAddChild(widget, node);
        } else if (!strcmp(info->properties[i].name,"input_mode")) {
            char *buf;
            buf = strstr(info->properties[i].value,"GTK_PANDA_ENTRY_");
            if (buf != NULL) { 
                buf += strlen("GTK_PANDA_ENTRY_");
            } else { 
                buf = "";
            }
            node = xmlNewNode(NULL, BAD_CAST(info->properties[i].name));
            xmlNodeAddContent(node, BAD_CAST(buf));
            xmlAddChild(widget, node);
        } else if (!strcmp(info->properties[i].name,"type")) {
            node = xmlNewNode(NULL, BAD_CAST(info->properties[i].name));
			if (!strcmp(info->properties[i].value,"GTK_WINDOW_POPUP")) {
            	xmlNodeAddContent(node, BAD_CAST("GTK_WINDOW_DIALOG"));
			} else {
            	xmlNodeAddContent(node, BAD_CAST("GTK_WINDOW_TOPLEVEL"));
			}
            xmlAddChild(widget, node);
        } else if (!strcmp(info->properties[i].name,"window_position")) {
            node = xmlNewNode(NULL, BAD_CAST("position"));
			if (!strcmp(info->properties[i].value,"GTK_WIN_POS_CENTER")) {
            	xmlNodeAddContent(node, BAD_CAST("GTK_WIN_POS_CENTER"));
			} else {
            	xmlNodeAddContent(node, BAD_CAST("GTK_WIN_POS_NONE"));
			}
            xmlAddChild(widget, node);
        } else if (!strcmp(info->properties[i].name,"events")) {
        } else if (!strcmp(info->properties[i].name,"extension_events")) {
        } else {
            node = xmlNewNode(NULL, BAD_CAST(info->properties[i].name));
            xmlNodeAddContent(node, BAD_CAST(info->properties[i].value));
            xmlAddChild(widget, node);
        }
    }

    for (i = 0; i < info->n_accels; i++) {
        node = xmlNewNode(NULL, BAD_CAST("accelerator"));
        xmlAddChild(widget, node);

        gchar *modifiers = modifier_string_from_bits (info->accels[i].modifiers);
	    child = xmlNewNode(NULL, BAD_CAST("modifiers"));
        if (modifiers == NULL || strlen(modifiers) == 0) {
            xmlNodeAddContent(child, BAD_CAST("0"));
        } else {
            xmlNodeAddContent(child, BAD_CAST(modifiers));
        }
        xmlAddChild(node, child);

		gchar key[64];
	    child = xmlNewNode(NULL, BAD_CAST("key"));
		sprintf(key,"GDK_%s",gdk_keyval_name(info->accels[i].key));
        xmlNodeAddContent(child, BAD_CAST(key));
        xmlAddChild(node, child);

	    child = xmlNewNode(NULL, BAD_CAST("signal"));
        xmlNodeAddContent(child, BAD_CAST(info->accels[i].signal));
        xmlAddChild(node, child);

        if (modifiers) {
            g_free (modifiers);
        }
    }

    for (i = 0; i < info->n_signals; i++) {
	    node = xmlNewNode(NULL, BAD_CAST("signal"));
        xmlAddChild(widget, node);

	    child = xmlNewNode(NULL, BAD_CAST("name"));
        xmlNodeAddContent(child, BAD_CAST(info->signals[i].name));
        xmlAddChild(node, child);

	    child = xmlNewNode(NULL, BAD_CAST("handler"));
        xmlNodeAddContent(child, BAD_CAST(info->signals[i].handler));
        xmlAddChild(node, child);

	    if (info->signals[i].object) {
	        child = xmlNewNode(NULL, BAD_CAST("data"));
            xmlNodeAddContent(child, BAD_CAST(info->signals[i].object));
            xmlAddChild(node, child);
        }

	    if (info->signals[i].after) {
	      child = xmlNewNode(NULL, BAD_CAST("after"));
          xmlNodeAddContent(child, BAD_CAST("True"));
          xmlAddChild(node, child);
        }
    }

    /* child widget */
    for (i = 0; i < info->n_children; i++) {
        dump_widget_panda(widget, info->children[i].child, &(info->children[i]), info);
    }
}

static xmlDoc *
glade_interface_make_doc_panda (GladeInterface *interface)
{
    xmlDoc *doc;
    xmlNode *root, *node1, *node2;
    gint i;
    gchar *name;

    doc = xmlNewDoc(BAD_CAST("1.0"));
    doc->standalone = FALSE;

    root = xmlNewNode(NULL, BAD_CAST("GTK-Interface"));
    xmlDocSetRootElement(doc, root);

    if (interface->n_toplevels > 0) {
      name = interface->toplevels[0]->name;
    } else {
      name = "empty";
    }
    node1 = xmlNewNode(NULL, BAD_CAST("project"));
	xmlAddChild(root, node1);

    node2 = xmlNewNode(NULL, BAD_CAST("name"));
	xmlNodeAddContent(node2, BAD_CAST(name));
	xmlAddChild(node1, node2);

    node2 = xmlNewNode(NULL, BAD_CAST("program_name"));
	xmlNodeAddContent(node2, BAD_CAST(name));
	xmlAddChild(node1, node2);
#if 0
    node2 = xmlNewNode(NULL, BAD_CAST("directory"));
	xmlNodeAddContent(node2, BAD_CAST(""));
	xmlAddChild(node1, node2);
#endif

    node2 = xmlNewNode(NULL, BAD_CAST("pixmaps_directory"));
	xmlNodeAddContent(node2, BAD_CAST("pixmaps"));
	xmlAddChild(node1, node2);

    node2 = xmlNewNode(NULL, BAD_CAST("gnome_support"));
	xmlNodeAddContent(node2, BAD_CAST("True"));
	xmlAddChild(node1, node2);

    node2 = xmlNewNode(NULL, BAD_CAST("gtkpanda_support"));
	xmlNodeAddContent(node2, BAD_CAST("False"));
	xmlAddChild(node1, node2);

    for (i = 0; i < interface->n_toplevels; i++) {
	    dump_widget_panda(root, interface->toplevels[i], NULL, NULL);
    }
    return doc;
}

static gboolean
eval_cb1(
  const GMatchInfo *info,
  GString *res,
  gpointer data)
{
  gchar *match;

  match = g_match_info_fetch(info, 1);
  g_string_append (res, "<");
  g_string_append (res, match);
  g_string_append (res, ">");
  g_string_append (res, "</");
  g_string_append (res, match);
  g_string_append (res, ">");
  g_free(match);
  return FALSE;
}

static gboolean
eval_cb2(
  const GMatchInfo *info,
  GString *res,
  gpointer data)
{
  gchar *match;

  match = g_match_info_fetch(info, 1);
  g_string_append (res, match);
  g_free(match);
  match = g_match_info_fetch(info, 2);
  g_string_append (res, match);
  g_free(match);
  return FALSE;
}

static void
glade_interface_buffer_panda (GladeInterface  *interface,
			gpointer        *ret,
			gint            *size)
{
    xmlDoc *doc;
    gchar *buf1;
    gchar *buf2;
    GRegex *reg;

    g_return_if_fail (interface != NULL);
    g_return_if_fail (ret       != NULL);
    g_return_if_fail (size      != NULL);

    doc = glade_interface_make_doc_panda (interface);
    xmlDocDumpFormatMemoryEnc(doc, (xmlChar **)&buf1,
			      size, "EUC-JP",  TRUE);

    reg = g_regex_new("<(.*)/>",G_REGEX_RAW,0,NULL); 
    buf2 = g_regex_replace_eval(reg,buf1,-1,0,0,eval_cb1,NULL,NULL);
    g_regex_unref(reg);
    g_free(buf1);
    if (buf2 != NULL) {
      reg = g_regex_new("(<\\?xml.*)encoding=\"EUC-JP\"(.*>)",G_REGEX_RAW,0,NULL); 
      *ret = g_regex_replace_eval(reg,buf2,-1,0,0,eval_cb2,NULL,NULL);
      g_regex_unref(reg);
      g_free(buf2);
      if (*ret != NULL) {
          *size = strlen(*ret);
      } else {
          *size = 0;
      }
    } else {
      *ret = NULL;
      *size = 0;
	}
    xmlFreeDoc(doc);
}

/***********************************
 * end of panda
 ***********************************/

static void
dump_widget(xmlNode *parent, GladeWidgetInfo *info, gint indent)
{
    xmlNode *widget;
    gint i, j;

    if (info == NULL) {
	widget = xmlNewNode(NULL, BAD_CAST("placeholder"));
	xmlAddChild(parent, widget);
	return;
    }

    widget = xmlNewNode(NULL, BAD_CAST("widget"));

    xmlSetProp(widget, BAD_CAST("class"), BAD_CAST(info->classname));
    xmlSetProp(widget, BAD_CAST("id"), BAD_CAST(info->name));
    xmlAddChild(parent, widget);
    xmlNodeAddContent(widget, BAD_CAST("\n"));

    for (i = 0; i < info->n_properties; i++) { 
	xmlNode *node;

	for (j = 0; j < indent + 1; j++)
	    xmlNodeAddContent(widget, BAD_CAST("  "));
	node = xmlNewNode(NULL, BAD_CAST("property"));
	xmlSetProp(node, BAD_CAST("name"), BAD_CAST(info->properties[i].name));
	if (info->properties[i].translatable)
	    xmlSetProp(node, BAD_CAST("translatable"), BAD_CAST("yes"));
	if (info->properties[i].has_context)
	    xmlSetProp(node, BAD_CAST("context"), BAD_CAST("yes"));
	if (info->properties[i].comment)
	    xmlSetProp(node, BAD_CAST("comments"), 
		       BAD_CAST(info->properties[i].comment));
	xmlNodeSetContent(node, BAD_CAST(info->properties[i].value));
	xmlAddChild(widget, node);
	xmlNodeAddContent(widget, BAD_CAST("\n"));
    }

    if (info->n_atk_props   != 0 ||
	info->n_atk_actions != 0 ||
	info->n_relations   != 0) {
	xmlNode *atk;

	for (j = 0; j < indent + 1; j++)
	    xmlNodeAddContent(widget, BAD_CAST("  "));
	atk = xmlNewNode(NULL, BAD_CAST("accessibility"));
	xmlAddChild(widget, atk);
	xmlNodeAddContent(widget, BAD_CAST("\n"));
	xmlNodeAddContent(atk, BAD_CAST("\n"));

	for (i = 0; i < info->n_atk_props; i++) {
	    xmlNode *node;

	    for (j = 0; j < indent + 2; j++)
		xmlNodeAddContent(atk, BAD_CAST("  "));
	    node = xmlNewNode(NULL, BAD_CAST("atkproperty"));
	    xmlSetProp(node, BAD_CAST("name"), BAD_CAST(info->atk_props[i].name));
	    if (info->atk_props[i].translatable)
		xmlSetProp(node, BAD_CAST("translatable"), BAD_CAST("yes"));
	    if (info->atk_props[i].has_context)
		xmlSetProp(node, BAD_CAST("context"), BAD_CAST("yes"));
	    if (info->atk_props[i].comment)
		xmlSetProp(node, BAD_CAST("comments"), 
			   BAD_CAST(info->atk_props[i].comment));
	    xmlNodeSetContent(node, BAD_CAST(info->atk_props[i].value));
	    xmlAddChild(atk, node);
	    xmlNodeAddContent(atk, BAD_CAST("\n"));
	}

	for (i = 0; i < info->n_atk_actions; i++) {
	    xmlNode *node;

	    for (j = 0; j < indent + 2; j++)
		xmlNodeAddContent(atk, BAD_CAST("  "));
	    node = xmlNewNode(NULL, BAD_CAST("atkaction"));
	    xmlSetProp(node, BAD_CAST("action_name"), 
		       BAD_CAST(info->atk_actions[i].action_name));
	    xmlSetProp(node, BAD_CAST("description"), 
		       BAD_CAST(info->atk_actions[i].description));
	    xmlAddChild(atk, node);
	    xmlNodeAddContent(atk, BAD_CAST("\n"));
	}

	for (i = 0; i < info->n_relations; i++) {
	    xmlNode *node;

	    for (j = 0; j < indent + 2; j++)
		xmlNodeAddContent(atk, BAD_CAST("  "));
	    node = xmlNewNode(NULL, BAD_CAST("atkrelation"));
	    xmlSetProp(node, BAD_CAST("target"), 
		       BAD_CAST(info->relations[i].target));
	    xmlSetProp(node, BAD_CAST("type"), 
		       BAD_CAST(info->relations[i].type));
	    xmlAddChild(atk, node);
	    xmlNodeAddContent(atk, BAD_CAST("\n"));
	}

	for (j = 0; j < indent + 1; j++)
	    xmlNodeAddContent(atk, BAD_CAST("  "));
    }

    for (i = 0; i < info->n_signals; i++) {
	xmlNode *node;

	for (j = 0; j < indent + 1; j++)
	    xmlNodeAddContent(widget, BAD_CAST("  "));

	node = xmlNewNode(NULL, BAD_CAST("signal"));
	xmlSetProp(node, BAD_CAST("name"), BAD_CAST(info->signals[i].name));
	xmlSetProp(node, BAD_CAST("handler"), BAD_CAST(info->signals[i].handler));
	if (info->signals[i].after)
	    xmlSetProp(node, BAD_CAST("after"), BAD_CAST("yes"));
	if (info->signals[i].lookup)
	    xmlSetProp(node, BAD_CAST("lookup"), BAD_CAST("yes"));
	if (info->signals[i].object)
	    xmlSetProp(node, BAD_CAST("object"), BAD_CAST(info->signals[i].object));
	xmlAddChild(widget, node);
	xmlNodeAddContent(widget, BAD_CAST("\n"));
    }

    for (i = 0; i < info->n_accels; i++) {
	xmlNode *node;
	gchar   *modifiers;

	modifiers = modifier_string_from_bits (info->accels[i].modifiers);

	for (j = 0; j < indent + 1; j++)
	    xmlNodeAddContent(widget, BAD_CAST("  "));

	node = xmlNewNode(NULL, BAD_CAST("accelerator"));
	xmlSetProp(node, BAD_CAST("key"), BAD_CAST(gdk_keyval_name(info->accels[i].key)));
	xmlSetProp(node, BAD_CAST("modifiers"), BAD_CAST(modifiers));
	xmlSetProp(node, BAD_CAST("signal"), BAD_CAST(info->accels[i].signal));
	xmlAddChild(widget, node);
	xmlNodeAddContent(widget, BAD_CAST("\n"));

	if (modifiers)
	    g_free (modifiers);
    }

    for (i = 0; i < info->n_children; i++) {
	xmlNode *child, *packing;
	GladeChildInfo *childinfo = &info->children[i];
	gint k;

	for (j = 0; j < indent + 1; j++)
	    xmlNodeAddContent(widget, BAD_CAST("  "));

	child = xmlNewNode(NULL, BAD_CAST("child"));
	if (childinfo->internal_child)
	    xmlSetProp(child, BAD_CAST("internal-child"), BAD_CAST(childinfo->internal_child));
	xmlAddChild(widget, child);
	xmlNodeAddContent(widget, BAD_CAST("\n"));
	xmlNodeAddContent(child, BAD_CAST("\n"));

	for (j = 0; j < indent + 2; j++)
	    xmlNodeAddContent(child, BAD_CAST("  "));
	dump_widget(child, childinfo->child, indent + 2);
	xmlNodeAddContent(child, BAD_CAST("\n"));

	if (childinfo->n_properties) {
	    for (j = 0; j < indent + 2; j++)
		xmlNodeAddContent(child, BAD_CAST("  "));
	    packing = xmlNewNode(NULL, BAD_CAST("packing"));
	    xmlAddChild(child, packing);
	    xmlNodeAddContent(packing, BAD_CAST("\n"));

	    for (k = 0; k < childinfo->n_properties; k++) { 
		xmlNode *node;

		for (j = 0; j < indent + 3; j++)
		    xmlNodeAddContent(packing, BAD_CAST("  "));
		node = xmlNewNode(NULL, BAD_CAST("property"));
		xmlSetProp(node, BAD_CAST("name"), BAD_CAST(childinfo->properties[k].name));
		if (childinfo->properties[k].translatable)
		    xmlSetProp(node, BAD_CAST("translatable"), BAD_CAST("yes"));
		if (childinfo->properties[k].has_context)
		    xmlSetProp(node, BAD_CAST("context"), BAD_CAST("yes"));
		if (childinfo->properties[k].comment)
		    xmlSetProp(node, BAD_CAST("comments"), 
			       BAD_CAST(childinfo->properties[k].comment));
		xmlNodeSetContent(node, BAD_CAST(childinfo->properties[k].value));
		xmlAddChild(packing, node);
		xmlNodeAddContent(packing, BAD_CAST("\n"));
	    }
	}

	if (childinfo->n_properties) {
	    for (j = 0; j < indent + 2; j++)
		xmlNodeAddContent(packing, BAD_CAST("  "));
	    xmlNodeAddContent(child, BAD_CAST("\n"));
	}

	for (j = 0; j < indent + 1; j++)
	    xmlNodeAddContent(child, BAD_CAST("  "));
    }

    for (j = 0; j < indent; j++)
	xmlNodeAddContent(widget, BAD_CAST("  "));
}


static xmlDoc *
glade_interface_make_doc (GladeInterface *interface)
{
    xmlDoc *doc;
    xmlNode *root, *comment;
    gint i;

    doc = xmlNewDoc(BAD_CAST("1.0"));
    doc->standalone = FALSE;
    xmlCreateIntSubset(doc, BAD_CAST("glade-interface"),
		       NULL, BAD_CAST("glade-2.0.dtd"));

    if (interface->comment)
    {
	comment = xmlNewComment(BAD_CAST (interface->comment));
	xmlDocSetRootElement(doc, comment);
    }
	
    root = xmlNewNode(NULL, BAD_CAST("glade-interface"));
    xmlDocSetRootElement(doc, root);

    xmlNodeAddContent(root, BAD_CAST("\n"));

    for (i = 0; i < interface->n_requires; i++) {
	xmlNode *node = xmlNewNode(NULL, BAD_CAST("requires"));

	xmlSetProp(node, BAD_CAST("lib"), BAD_CAST(interface->requires[i]));

	xmlNodeAddContent(root, BAD_CAST("  "));
	xmlAddChild(root, node);
	xmlNodeAddContent(root, BAD_CAST("\n"));
    }

    for (i = 0; i < interface->n_toplevels; i++) {
	xmlNodeAddContent(root, BAD_CAST("  "));
	dump_widget(root, interface->toplevels[i], 1);
	xmlNodeAddContent(root, BAD_CAST("\n"));
    }
    return doc;
}

static void
glade_interface_buffer (GladeInterface  *interface,
			gpointer        *buffer,
			gint            *size)
{
    xmlDoc *doc;
    g_return_if_fail (interface != NULL);
    g_return_if_fail (buffer    != NULL);
    g_return_if_fail (size      != NULL);

    doc = glade_interface_make_doc (interface);
    xmlDocDumpFormatMemoryEnc(doc, (xmlChar **)buffer,
			      size, "UTF-8",  TRUE);
    xmlFreeDoc(doc);
}

/**
 * glade_parser_interface_dump
 * @interface: the GladeInterface
 * @filename: the filename to write the interface data to.
 * @error: a #GError for error handleing.
 *
 * This function dumps the contents of a GladeInterface into a file as
 * XML.  It is used by glade to write glade files.
 *
 * Returns whether the write was successfull or not.
 */
gboolean
glade_parser_interface_dump (GladeInterface *interface,
			     const gchar *filename,
			     GError **error)
{
	gpointer buffer;
	gint     size;
    gchar *env;
   
    env = getenv("OUTPUT_GLADE_3_FORMAT");

    if (env != NULL && strlen(env) > 0) {
	    glade_interface_buffer (interface, &buffer, &size);
    } else {
	    glade_interface_buffer_panda (interface, &buffer, &size);
    }
	
	if (buffer == NULL)
	{
		g_set_error (error, G_FILE_ERROR, G_FILE_ERROR_NOMEM,
			     _("Could not allocate memory for interface"));
		return FALSE;
	}

    g_file_set_contents(filename, buffer, size, error);
	xmlFree (buffer);
	
	return (*error == NULL) ? TRUE : FALSE;
}

G_CONST_RETURN gchar *
glade_parser_pvalue_from_winfo (GladeWidgetInfo *winfo,
				const gchar     *pname)
{
	gchar *dup_name = g_strdup (pname);
	gint i;

	/* Make '_' & '-' synonymous here for convenience.
	 */
	glade_util_replace (dup_name, '-', '_');
	for (i = 0; i < winfo->n_properties; i++)
	{
		if (!strcmp (pname, winfo->properties[i].name) ||
		    !strcmp (dup_name, winfo->properties[i].name))
			return winfo->properties[i].value;
	}
	return NULL;
}


#if 0
int
main(int argc, char **argv) {
    gtk_init(&argc, &argv);
    if (argc > 1) {
	GladeInterface *interface = glade_parser_parse_file(argv[1]);
	g_message("output: %p", interface);
	if (interface) {
	    glade_interface_dump(interface, "/dev/stdout");
	    glade_interface_destroy(interface);
	}
    } else
	g_message("need filename");
    return 0;
}
#endif
