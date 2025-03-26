import java.util.ArrayList;
import java.util.Random;
import java.util.Scanner;

public class OpenWorld {

	private int northSouthBoundary, eastWestBoundary;
	private Player player;
	private PlayerLocation playerLocation;
	private ArrayList<Encounter> encounters = new ArrayList<>();
	private boolean gameOver = false;
	private boolean willFlatten = false;
	private boolean willSpawnWolves = false;
	private LogAnalytics analyticsLogger;

	public OpenWorld() {
	}

	private void initialise(int northSouthBoundary, int eastWestBoundary) {
		this.northSouthBoundary = northSouthBoundary;
		this.eastWestBoundary = eastWestBoundary;
		this.playerLocation = new PlayerLocation(northSouthBoundary, eastWestBoundary);
		this.player = new Player(100);
		this.analyticsLogger = LogAnalytics.getLogger(this);

		playerLocation.changeCoordinates(new Coordinates(0, 0));
		initialiseEncounters();
		for (Encounter encounter : encounters) {
			encounter.printEncounterDetails();
		}
		System.out.println("---------------------------");
		playerMove();
	}

	private void playerMove() {
		Scanner scanner = new Scanner(System.in);
		String userInput = "";

		while (userInput != "exit") {
			if (gameOver || player.getHealth() <= 0) {
				System.out.println("The game is over. Exiting...");
				return;
			}

			System.out.println("Player has " + player.getHealth() + " health left!");
			System.out.println("Enter the direction you want to move in:");
			userInput = scanner.nextLine().toLowerCase();

			switch (userInput) {
			case "north":
				playerLocation.changeCoordinates(1, 0);
				break;
			case "south":
				playerLocation.changeCoordinates(-1, 0);
				break;
			case "east":
				playerLocation.changeCoordinates(0, 1);
				break;
			case "west":
				playerLocation.changeCoordinates(0, -1);
				break;
			}

			if (willFlatten || willSpawnWolves) {
				changePlayerSurroundings();
			}
		}
		System.out.println("---------------------------");
	}

	public void setGameOver(boolean status) {
		this.gameOver = status;
	}

	public PlayerLocation getPlayerLocation() {
		return this.playerLocation;
	}

	private void initialiseEncounters() {
		Random rand = new Random();
		EncounterFactory factory = new EncounterFactory();
		for (int northSouth = 0; northSouth < this.northSouthBoundary; northSouth++) {
			for (int eastWest = 0; eastWest < this.eastWestBoundary; eastWest++) {
				this.encounters.add(factory.createEncounter(rand.nextInt(6), this, player, northSouth, eastWest));
			}
		}
	}

	private Encounter getEncounterByLocation(Coordinates location) {
		for (Encounter encounter : encounters) {
			if (encounter.getEncounterByLocation(location) != null) {
				return encounter;
			}
		}
		return null;
	}

	public void alertFlattening() {
		willFlatten = true;
	}

	public void alertWolves() {
		willSpawnWolves = true;
	}

	private void changePlayerSurroundings() {
		for (Coordinates adjacentSpace : playerLocation.getSurroundings()) {
			Encounter adjacentEncounter = getEncounterByLocation(adjacentSpace);
			if (adjacentEncounter != null) {
				playerLocation.removeObserver(adjacentEncounter);
				if (willFlatten) {
					encounters.add(
							new FlatlandsEncounter(this, player, adjacentEncounter.encounterCoordinates.getNorthSouth(),
									adjacentEncounter.encounterCoordinates.getEastWest()));
				} else if (willSpawnWolves) {
					encounters
							.add(new WolfEncounter(this, player, adjacentEncounter.encounterCoordinates.getNorthSouth(),
									adjacentEncounter.encounterCoordinates.getEastWest()));
				}
				encounters.remove(adjacentEncounter);
			}
		}
		if (willFlatten) {
			willFlatten = false;
		} else if (willSpawnWolves) {
			willSpawnWolves = false;
		}
	}

	public static void main(String[] args) {
		OpenWorld world = new OpenWorld();
		world.initialise(10, 10);

	}

}
