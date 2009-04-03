#include "iosim.h"
#include <stdlib.h>

void symtest_initialize() {
	IOSIM_fd[1] = malloc(sizeof(sym_file_stream_t));
	IOSIM_fd[1]->offset = 0;
	IOSIM_fd[1]->fd = 1;
	IOSIM_fd[1]->sym_file = malloc(sizeof(sym_file_t));
	IOSIM_fd[1]->sym_file->contents = NULL;
	IOSIM_fd[1]->sym_file->stat.st_size = 0;
	stdout = IOSIM_fd[1];

	sym_file_t* sed = IOSIM_addfile("linecnt.sed", 0);
	sed->contents = "=";
	sed->stat.st_size = 1;

	sym_file_t* input = IOSIM_addfile("linecnt.inp", 0);
	input->contents = "A dialogue on poverty\n\n        On the night when the rain beats,\n        Driven by the wind,\n        On the night when the snowflakes mingle\n        With a sleety rain,\n        I feel so helplessly cold.\n        I nibble at a lump of salt,\n        Sip the hot, oft-diluted dregs of _sake_;\n        And coughing, snuffling,\n        And stroking my scanty beard,\n        I say in my pride,\n        "There's none worthy, save I!"\n        But I shiver still with cold.\n        I pull up my hempen bedclothes,\n        Wear what few sleeveless clothes I have,\n        But cold and bitter is the night!\n        As for those poorer than myself,        \n        Their parents must be cold and hungry,\n        Their wives and children beg and cry.\n        Then, how do you struggle through life?\n        \n        Wide as they call the heaven and earth,\n        For me they have shrunk quite small;\n        Bright though they call the sun and moon,\n        They never shine for me.\n        Is it the same with all men,\n        Or for me alone?\n        By rare chance I was born a man\n        And no meaner than my fellows,\n        But, wearing unwadded sleeveless clothes\n        In tatters, like weeds waving in the sea,\n        Hanging from my shoulders,\n        And under the sunken roof,\n        Within the leaning walls,\n        Here I lie on straw\n        Spread on bare earth,\n        With my parents at my pillow,\n        And my wife and children at my feet,\n        All huddled in grief and tears.\n        No fire sends up smoke\n        At the cooking-place,\n        And in the cauldron\n        A spider spins its web.\n        With not a grain to cook,\n        We moan like the night thrush.\n        Then, "to cut," as the saying is,\n        "The ends of what is already too short,"\n        The village headman comes,\n        With rod in hand, to our sleeping place,\n        Growling for his dues.\n        Must it be so hopeless --\n        The way of this world?\n\n        -- Yamanoue Okura";
	input->stat.st_size = 1987;
}