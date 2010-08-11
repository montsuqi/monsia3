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
empty (GObject *container, GladeCreateReason reason)
{
}

/* Catalog init function */
void
glade_gtkpanda2_init (void)
{
	gint argc = 1;
	gchar *argv[2] = {"glade-3", NULL};
	gchar **argvv = argv;
	
	gtk_init(&argc,&argvv);
	gtk_panda_init(&argc,&argvv);
}
