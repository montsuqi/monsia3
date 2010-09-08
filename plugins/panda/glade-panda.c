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
		 "pandaentry", "pandacomboentry", FALSE, reason);
}

GObject *
glade_gtk_panda_combo_get_internal_child (GladeWidgetAdaptor *adaptor,
					      GObject *object, 
					      const gchar *name)
{
	GObject *child = NULL;
	g_return_val_if_fail (GTK_IS_COMBO_BOX_ENTRY (object), NULL);
	
	if (strcmp ("pandaentry", name) == 0)
		child = G_OBJECT (gtk_bin_get_child (GTK_BIN (object)));

	return child;
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
