
public abstract class Encounter implements PlayerObserver {

	protected final OpenWorld game;
	protected final Player player;
	protected final Coordinates encounterCoordinates;
	protected final String name;
	protected final String nearbyAlert;

	public Encounter(OpenWorld game, Player player, String name, String nearbyAlert, int northSouth, int eastWest) {
		this.game = game;
		this.player = player;
		this.name = name;
		this.nearbyAlert = nearbyAlert;
		this.encounterCoordinates = new Coordinates(northSouth, eastWest);
		game.getPlayerLocation().registerObserver(this);
	}

	public Coordinates getEncounterCoordinates() {
		return encounterCoordinates;
	}

	public void printEncounterDetails() {
		System.out.println(name + " " + encounterCoordinates.toString());
	}

	public void nearbyMessage(Coordinates playerLocation) {
		if (encounterCoordinates.isNorth(playerLocation)) {
			System.out.println("To the North " + nearbyAlert);
		}
		if (encounterCoordinates.isSouth(playerLocation)) {
			System.out.println("To the South " + nearbyAlert);
		}
		if (encounterCoordinates.isEast(playerLocation)) {
			System.out.println("To the East " + nearbyAlert);
		}
		if (encounterCoordinates.isWest(playerLocation)) {
			System.out.println("To the West " + nearbyAlert);
		}
	}

	public Encounter getEncounterByLocation(Coordinates location) {
		if (encounterCoordinates.isHere(location)) {
			return this;
		}
		return null;
	}

	@Override
	public String toString() {
		return name + " at " + encounterCoordinates;
	}

}
