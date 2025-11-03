//
//	Mainn Framework definitions
//
#ifndef __FRAMEWORK_H__
#define __FRAMEWORK_H__

#include <arch/zxn.h>
#include <intrinsic.h>

#define BREAK { intrinsic_emit(0xFD); intrinsic_emit(0x00); }
#define EXIT { intrinsic_emit(0xDD); intrinsic_emit(0x00); }
#define NextReg(r,v)	ZXN_NEXTREG_helper(r,v)
#define NextRegA(r,var)	ZXN_NEXTREGA_helper(r,var)

typedef	uint8_t		uint8;
typedef	int8_t		int8;
typedef	uint16_t	uint16;
typedef	int16_t		int16;
typedef	uint32_t	uint32;
typedef	int32_t		int32;

#define	SetState(state)	GameState=state

typedef enum eGameStateType
{
	State_InitFrontEnd = 1,		// init the front end
	State_FrontEnd,			// Process front end
	State_QuitFrontEnd,		// quit the front end

	State_InitGame,			// ini the game
	State_Game,			// process the game
	State_QuitGame,			// quot the game
}eGameState;

typedef	struct SHWSprite
{
	uint8	X;
	uint8	Y;
	uint8	Palette_FlipRotate;
	uint8	Visible_1_Name;
	uint8	H_N6_Scale_Y8;

}SHWSprite,*PSHWSprite;


#endif // __FRAMEWORK_H__


