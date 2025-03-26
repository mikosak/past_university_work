import java.util.ArrayList;

public class PlayerLocation implements PlayerSubject {
	private Coordinates playerCoordinates;
	private final int northSouthBoundary;
	private final int eastWestBoundary;
	private ArrayList<PlayerObserver> observers = new ArrayList<>();

	public PlayerLocation(int northSouthBoundary, int eastWestBoundary) {
		this.northSouthBoundary = northSouthBoundary;
		this.eastWestBoundary = eastWestBoundary;
	}

	@Override
	public void registerObserver(PlayerObserver observerToRegister) {
		observers.add(observerToRegister);
	}

	@Override
	public void removeObserver(PlayerObserver observerToRemove) {
		observers.remove(observerToRemove);
	}

	@Override
	public void notifyObservers() {
		for (PlayerObserver observer : observers) {
			observer.update(playerCoordinates);
		}
	}

	public void changeCoordinates(int northSouthChange, int eastWestChange) {
		changeCoordinates(new Coordinates(playerCoordinates.getNorthSouth() + northSouthChange,
				playerCoordinates.getEastWest() + eastWestChange));
	}

	public void changeCoordinates(Coordinates updateCoordinates) {
		if (updateCoordinates.getNorthSouth() >= 0 && updateCoordinates.getNorthSouth() <= northSouthBoundary
				&& updateCoordinates.getEastWest() >= 0 && updateCoordinates.getEastWest() <= eastWestBoundary) {
			playerCoordinates = updateCoordinates;
			System.out.println("You are at location: " + playerCoordinates.toString());
		} else {
			System.out.println("Out of bounds move attempted - position has not changed, you are at: "
					+ playerCoordinates.toString());
		}
		notifyObservers(); // notify even if unchanged position, requirement for logging
	}

	public ArrayList<Coordinates> getSurroundings() {
		ArrayList<Coordinates> surroundings = new ArrayList<>();
		surroundings.add(new Coordinates(playerCoordinates.getNorthSouth() + 1, playerCoordinates.getEastWest()));
		surroundings.add(new Coordinates(playerCoordinates.getNorthSouth() - 1, playerCoordinates.getEastWest()));
		surroundings.add(new Coordinates(playerCoordinates.getNorthSouth(), playerCoordinates.getEastWest() + 1));
		surroundings.add(new Coordinates(playerCoordinates.getNorthSouth(), playerCoordinates.getEastWest() - 1));
		return surroundings;
	}

}
