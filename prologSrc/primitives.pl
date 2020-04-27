:-module(primitives,[load_img/2,write_img/2,release_img/1,subimg/4,copy_img/2,draw_objects_/4,
        draw_circles/1,show_objects/1,show_img/1,draw_rect/1,draw_grid/1,draw_grid_triangle/1, 
		get_color_rgb/3,main_color_rgb/2,
		find_all_polygons/4,area/2,
		read_single_text/4,read_single_number/4,read_text/4,write_text/3]).

load_dll:-		
	load_foreign_library(foreign('../primitives/prologPrimitives.so')).

:-load_dll.
