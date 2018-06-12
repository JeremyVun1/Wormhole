unit WH_Module_Interface;

///////////
//Public
///////////
interface

uses WH_Types_Shared, WH_Types_Interface, WH_Module_Battle;

//Interface module between the menus and the maingame
//Menu module sends data to the Maingame module and receives data back when the game finishes
//MainGameInterface(SendData): ReceiveData
function MainGameInterface(const Send: SendData): ReceiveData;


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
	Result := MainGame(Send.Difficulty, Send.ShipClass);
end;

end.