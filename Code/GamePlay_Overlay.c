//  ***************************************************************************************
//
//                              Simple Game framework
//
//  ***************************************************************************************
#include <arch/zxn.h>           // ZX Spectrum Next architecture specfic functions
#include <stdint.h>             // standard names for ints with no ambiguity 
#include <z80.h>
#include <im2.h>
#include <intrinsic.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include <input.h>
#include <errno.h>

#include "Kernel.h"
#include "FrontEnd.h"
#include "data.h"

#include "GamePlay.h"

#pragma output CRT_ORG_CODE = 0x0000


    uint16  counter;

//  ***************************************************************************************
//  Init the Game
//  ***************************************************************************************
void GP_Init( void )
{
    ClsATTR(7);
    ClsULA();
    InitSpriteData();

    // Set game process state when ready
    SetState(State_Game);
} 

//  ***************************************************************************************
//  Process the game
//  ***************************************************************************************
void GP_Process( void )
{

    if( Keys[VK_2]!=0)
    {
        Keys[VK_2] = 0;
        SetState(State_QuitGame);
    }


    // Test CSpect's DebugPrint
    if( Keys[VK_0]!=0)
    {
        Keys[VK_0] = 0;
        DebugPrint("Hello World 0x%4X, %s, AF=%raf HL=%rhl DE=%rde %d",0x1234, "Hello World2", 0x1234);
    }
    counter++;
} 

//  ***************************************************************************************
//  Render the game
//  ***************************************************************************************
void GP_Render( void )
{
    Print(0,48,"GP_Render - [2] to Quit\n");
} 

//  ***************************************************************************************
//  Quiit the game and cleanup
//  ***************************************************************************************
void GP_Quit( void )
{
    Print(0,58,"GP_Quit\n");

    // Set game process state when ready
    SetState(State_InitFrontEnd);
} 





