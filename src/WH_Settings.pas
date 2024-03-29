unit WH_Settings;

interface

const
	//VIEWPORT
	WINDOW_WIDTH = 800;
	WINDOW_HEIGHT = 600;
	PLAY_WIDTH = 1000;
	PLAY_HEIGHT = 1000;
	FPS = 60;

	//DRAWSCALE
	LINE_LENGTH = WINDOW_WIDTH div 65;

	//LEVEL
	STAR_COUNT = PLAY_WIDTH div 5;
	MAX_STAR_SIZE = 5;
	BASE_STAR_SIZE = 1;
	TWINKLE_PORTION = 40;
	TWINKLE_RATE = 6; //per second
	NPC_COUNT = 9;
	NOVICE_MODIFIER = 0.6;
	INTERMEDIATE_MODIFIER = 0.75;
	EXPERT_MODIFIER = 0.9;
	SPIN_DECAY = 0.977;
	SPEED_DECAY = 0.977;
	MAX_EXPLOSION_VELOCITY = 8;
	MAX_EXPLOSION_TURN_RATE = 300; //degrees per second
	LOOT_TURN_RATE = 2; //degrees per second
	PI = 3.1416;
	WORMHOLE_BASE_RADIUS = PLAY_WIDTH/15;
	WORMHOLE_BASE_HEALTH = 60;
	WORMHOLE_CONSTANT_GROWTH = 1; //per second
	WORMHOLE_GROWTH_RATE = 3; //Percent grwoth per second
	NUMBER_POPUP_VELOCITY = 1;
	NUMBER_POPUP_EXPIRY = 4; //in seconds
	AGRO_RANGE = WINDOW_WIDTH/2;
	SPAWN_TIME = 5; // in seconds
	NPC_FIRE_RATE = 0.33; // shots per second

	//SHIP
	LIGHT_MODIFIER = 1.5;
	MEDIUM_MODIFIER = 1.0;
	HEAVY_MODIFIER = 0.6;
	LIGHT_HEALTH = 5;
	MEDIUM_HEALTH = 10;
	HEAVY_HEALTH = 20;
	COLOR_FLASH_RATE = 4; //per second
	BUFF_DURATION = 5; //in seconds
	BUFF_SPEED_MODIFIER = 1.4;
	BUFF_DAMAGE_MODIFIER = 3;

	//MOVEMENT
	BASE_MAX_VELOCITY = WINDOW_WIDTH/270;
	BASE_ACCELERATION = BASE_MAX_VELOCITY/45;
	BASE_TURN_RATE = 160; // degrees per second
	BASE_STRAFE_MODIFIER = 1.2;
	BASE_REVERSE_MODIFIER = 0.8;
	BASE_FRICTION = 0.996;
	PLAYER_SPEED_BUFF = 1.25;
	COLLIDE_TIME_OUT = 2; // seconds
	WORMHOLE_COLLIDE_TIME_OUT = COLLIDE_TIME_OUT/4; // in seconds

	//BALLISTIC
	BALLISTIC_DAMAGE = 1;
	BALLISTIC_FIRE_RATE = 2; //shots per second
	BALLISTIC_MAX_VELOCITY = WINDOW_WIDTH/160;
	BALLISTIC_EXPIRY = 3; //seconds
	BALLISTIC_LENGTH = LINE_LENGTH/4;

	//MISSILE
	MISSILE_MAX_VELOCITY = WINDOW_WIDTH/120;
	MISSILE_ACCEL = MISSILE_MAX_VELOCITY/30;
	MISSILE_TURN_RATE = 320;
	MISSILE_FIRE_RATE = 0.5;
	MISSILE_EXPIRY = 5;
	MISSILE_DAMAGE = 2;
	MISSILE_LOCK_RANGE = AGRO_RANGE * 1.5;

implementation

end.