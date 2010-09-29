/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * Copyright (C) 2006 Juan Pablo Ugarte.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 * Authors:
 *   Juan Pablo Ugarte <juanpablougarte@gmail.com>
 */

#include <config.h>

#include <gladeui/glade.h>
#include <gladeui/glade-editor-property.h>

#include <glade-gtk.h>

#include <gtkpanda/gtkpanda.h>

/* This function does absolutely nothing
 * (and is for use in overriding post_create functions).
 */
void
empty (GladeWidgetAdaptor *adaptor,
	GObject *container, 
	GladeCreateReason reason)
{
}

void
glade_gtk_panda_combo_post_create (GladeWidgetAdaptor *adaptor,
				       GObject            *object, 
				       GladeCreateReason   reason)
{
	GladeWidget *gcombo = glade_widget_get_from_gobject (object);

	/* Chain up */
	GWA_GET_CLASS (GTK_TYPE_CONTAINER)->post_create (adaptor, object, reason);

	glade_widget_adaptor_create_internal
		(gcombo, G_OBJECT (GTK_BIN (object)->child),
		 "entry", "pandacombo", FALSE, reason);
}

#if 0
void
glade_gtk_panda_clist_add_child (GladeWidgetAdaptor  *adaptor,
				GObject	*parent,
				GObject	*child)
{
	GtkPandaCList *clist = GTK_PANDA_CLIST(parent);
	GtkTreeViewColumn *column;
	GList *list = NULL;
	int i;

	for(i = 0; i < gtk_panda_clist_get_n_columns(clist); i++) {
		column = gtk_tree_view_get_column(GTK_TREE_VIEW(clist), i);
		if (gtk_tree_view_column_get_widget(column) == NULL) {
			gtk_tree_view_column_set_widget(column,GTK_WIDGET(child));
		}
	}
}

void
glade_gtk_panda_clist_remove_child (GladeWidgetAdaptor  *adaptor,
				GObject	*parent,
				GObject	*child)
{
	GtkPandaCList *clist = GTK_PANDA_CLIST(parent);
	GtkTreeViewColumn *column;
	GtkWidget *_child;
	GList *list = NULL;
	int i;

	for(i = 0; i < gtk_panda_clist_get_n_columns(clist); i++) {
		column = gtk_tree_view_get_column(GTK_TREE_VIEW(clist), i);
		_child = gtk_tree_view_column_get_widget(column);
		if (GTK_WIDGET(child) == GTK_WIDGET(_child)) {
			gtk_tree_view_column_set_widget(column,NULL);
		}
	}
}
#endif

GList *
glade_gtk_panda_clist_get_children (GladeWidgetAdaptor  *adaptor,
				  GObject        *object)
{
	GladeWidget *gclist = glade_widget_get_from_gobject (object);
	GtkPandaCList *clist = GTK_PANDA_CLIST(object);
	GtkTreeViewColumn *column;
	GtkWidget *child;
	GList *list = NULL;
	int i;

	for(i = 0; i < gtk_panda_clist_get_n_columns(clist); i++) {
		column = gtk_tree_view_get_column(GTK_TREE_VIEW(clist), i);
		child = gtk_tree_view_column_get_widget(column);
		if (child != NULL) {
			list = g_list_append(list,child);
		}
	}
	return list;
}

void
glade_gtk_panda_clist_set_property (GladeWidgetAdaptor *adaptor,
			    GObject            *object, 
			    const gchar        *id,
			    const GValue       *value)
{
	GladeWidget *gclist = glade_widget_get_from_gobject (object);
	GtkPandaCList *clist = GTK_PANDA_CLIST(object);
	GtkTreeViewColumn *column;
	GtkWidget *child;
	int i;

	g_object_set_property(object,id,value);

	if (!strcmp (id, "n-columns")) {
		for(i = 0; i < gtk_panda_clist_get_n_columns(clist); i++) {
			column = gtk_tree_view_get_column(GTK_TREE_VIEW(clist), i);
			child = gtk_tree_view_column_get_widget(column);
			if (child != NULL) {
				glade_widget_adaptor_create_internal
					(gclist, G_OBJECT (child),
					 "child", "pandaclist", FALSE, GLADE_CREATE_USER);
			}
		}
	}
}

/* Catalog init function */
void
glade_panda_init (void)
{
	gint argc = 1;
	gchar *argv[2] = {"glade-3", NULL};
	gchar **argvv = argv;
	
	gtk_init(&argc,&argvv);
	gtk_panda_init(&argc,&argvv);
}

