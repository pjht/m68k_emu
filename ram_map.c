#include <stdint.h>

uint8_t find_all_ram_cards(uint8_t* buf /* a1 */) {
	uint8_t len = 0; // move.b 0, d0
	void* curr_card = (void*)0xff0000; // a0 = 0xff0000
	while(1) { // farc_loop:
		curr_card += 0x100; // lea (0x100, a0), a0 
		uint8_t type = *((uint8_t*)(curr_card+0xff)); // move.b (0xff, a0), d0
		if (type == 0) { // beq.b farc_done
			break;
		}
		if (type != 2) { // cmp.b #0x2, d0; bne.b farc_loop
    	continue; 
		}
		*buf = (uint8_t)((uint32_t)curr_card>>16);
		buf += 1; // inc a1
		len += 1; // inc d1
	} // bra.b farc_loop
	// farc_done:
	return len;
}

void sort_ram_cards(uint8_t* buf) {
	uint8_t len = find_all_ram_cards(buf);
	// Insertion sort buf using the aligment of the cards
	uint8_t i = 1;
	while(1) {
		if (i >= len) {
			break;
		}
		uint8_t num = buf[i];
		void* card = (void*)((uint32_t)num<<16);
		*((uint32_t*)card)=0xfffffffe;
		uint32_t x = ~(*((uint32_t*)card)) + 1;
		uint8_t j = i - 1;
		while(1) {
			if (j == 0) {
				break;
			}
			uint8_t num = buf[j];
			void* card = (void*)((uint32_t)num<<16);
			*((uint32_t*)card)=0xfffffffe;
			uint32_t aj = ~(*((uint32_t*)card)) + 1;
			if (aj <= x) {
				break;
			}
			buf[j + 1] = buf[j];
			j = j - 1;
		}
		buf[j + 1] = buf[i];
		i = i + 1;
	}
}
