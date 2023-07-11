#include <stdint.h>
#include "hoehrmann_utf8.c"

#include <stdio.h>

/* Decode a UTF8-encoded string to UCS4 using the Höhrmann decoder. Any
 * invalid bytes are stored using GHC 7.4's encoding for mixed-use strings.
 *
 * 'len' is the length of both 'utf8' and 'out'.
 *
 * Returns the number of items written to *out.
 */
int hsoptions_decode_string(uint8_t *utf8, uint32_t *out, int len)
{
	uint32_t state = UTF8_ACCEPT;
	uint8_t *utf8_end = utf8 + len;
	uint32_t codepoint = 0;
	uint32_t *out_orig = out;
	uint8_t *started_accept = utf8;
	
	while (utf8 < utf8_end)
	{
#if defined(__i386__) || defined(__x86_64__)
		/* Performance improvement from Bryan O'Sullivan.
		 *
		 * If some chunk of bytes is ASCII, it can be quickly pushed
		 * off into the output buffer without going through the UTF8
		 * decoder. On little-endian systems, pure-ASCII text will
		 * have a mask of 0x80808080.
		 */
		if (state == UTF8_ACCEPT)
		{
			while (utf8 < utf8_end - 4)
			{
				codepoint = *((uint32_t *) utf8);
				if ((codepoint & 0x80808080) != 0)
				{ break; }
				
				utf8 += 4;
				*out++ = codepoint & 0xFF;
				*out++ = (codepoint >> 8) & 0xFF;
				*out++ = (codepoint >> 16) & 0xFF;
				*out++ = (codepoint >> 24) & 0xFF;
			}
			started_accept = utf8;
		}
#endif
		switch (decode(&state, &codepoint, *utf8))
		{
		case UTF8_ACCEPT:
			/* Read a codepoint succesfully. Copy it out and
			 * continue.
			 */
			*out++ = codepoint;
			started_accept = utf8++;
			break;
		case UTF8_REJECT:
			/* The first byte of a new character was invalid.
			 * Escape it and begin again.
			 */
			if (started_accept == utf8)
			{
				*out++ = (uint32_t) (*utf8++) + 0xDC00;
				started_accept = utf8;
			}
			
			/* The decoder consumed some bytes before failing.
			 * Backtrack to escape everything after the last
			 * successfully read codepoint.
			 */
			else while (started_accept < utf8)
			{
				*out++ = (uint32_t) (*started_accept++) + 0xDC00;
			}
			
			/* Continue looking for valid UTF8 sequences in the
			 * input buffer.
			 */
			state = UTF8_ACCEPT;
			break;
		default:
			/* Decoder needs more input */
			utf8++;
			break;
		}
	}
	
	/* See comments in case UTF8_REJECT. */
	if (state != UTF8_ACCEPT)
	{
		if (started_accept == utf8)
		{
			*out++ = (uint32_t) (*utf8) + 0xDC00;
		}
		else while (started_accept < utf8)
		{
			*out++ = (uint32_t) (*started_accept++) + 0xDC00;
		}
	}
	
	return out - out_orig;
}
