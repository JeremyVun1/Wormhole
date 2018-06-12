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

//Read in last used game settings from file
//LoadLastSettings(Send: SendData);
procedure LoadLastSettings(var Send: SendData);

///////////
//Private
///////////
implementation

//////////////////
// MainGame/Menus Interface
//////////////////

function SettingsFilename(): String;
var
	subDir: String;
begin
	subDir := 'dat\';
	Result := Concat(subDir, 'settings.dat');
end;

procedure WriteSettings(const Send: SendData);
var
	SettingsFile: TextFile;
	filename: String;
begin
	filename := SettingsFilename();

	if FileExists(PathToResource(Filename)) then
	begin
		try
			WriteLn('Writing to ', PathToResource(FileName));
			AssignFile(SettingsFile, PathToResource(Filename));
			ReWrite(SettingsFile);

			//Write settings to file
			WriteLn(SettingsFile, Send.ShipClass);
			WriteLn(SettingsFile, Send.Difficulty);
			WriteLn(SettingsFile, Send.RotationControl);
		except
			WriteLn('WriteSettings() - File Handling Error');
		end;
		CloseFile(SettingsFile);
	end
	else
	begin
		WriteLn('WriteSettings() - File does not exist');
	end;
end;

//Interface module between the menus and the maingame
//Menu module sends data to the Maingame module and receives data back when the game finishes
//MainGameInterface(SendData): ReceiveData
function MainGameInterface(const Send: SendData): ReceiveData;
begin
	//Start MainGame
	Result := MainGame(Send);

	//write game send settings to file
	WriteSettings(Send);
end;

procedure LoadLastSettings(var Send: SendData);
var
	SettingsFile: TextFile;
	filename: String;
begin
	filename := SettingsFilename();
	if FileExists(PathToResource(Filename)) then
	begin
		try
			WriteLn('Reading ', PathToResource(FileName));
			AssignFile(SettingsFile, PathToResource(Filename));
			Reset(SettingsFile);

			//read settings from file
			ReadLn(SettingsFile, Send.ShipClass);
			ReadLn(SettingsFile, Send.Difficulty);
			ReadLn(SettingsFile, Send.RotationControl); 
		except
			WriteLn('LoadLastSettings() - File Handling Error');
		end;
		CloseFile(SettingsFile);
	end
	else
	begin
		WriteLn('LoadLastSettings() - File does not exist');
	end;
end;

end.