unit WH_Module_Interface;

///////////
//Public
///////////
interface

uses SwinGame, SysUtils, WH_Types_Shared, WH_Types_Interface, WH_Module_Battle;

//Interface module between the menus and the maingame
//Menu module sends data to the Maingame module and receives data back when the game finishes
//MainGameInterface(SendData): ReceiveData
function MainGameInterface(const Send: SendData): ReceiveData;

//Returns default settings for starting the game module
//DefaultSettings(): SendData
function DefaultSettings(): SendData;

///////////
//Private
///////////
implementation

//////////////////
// MainGame/Menus Interface
//////////////////

//Interface module between the menus and the maingame
//Menu module sends data to the Maingame module and receives data back when the game finishes
//MainGameInterface(SendData): ReceiveData
function MainGameInterface(const Send: SendData): ReceiveData;
begin
	//Start MainGame
	Result := MainGame(Send);
end;

function DefaultSettings(): SendData;
begin
	Result.Difficulty := Novice;
	Result.ShipClass := LightShip;
	Result.ShipControl := Keyboard;
end;

end.