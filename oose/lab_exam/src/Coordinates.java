
public class Coordinates {

	private int northSouth;
	private int eastWest;

	public Coordinates(int northSouth, int eastWest) {
		this.setNorthSouth(northSouth);
		this.setEastWest(eastWest);
	}

	public int getNorthSouth() {
		return northSouth;
	}

	public void setNorthSouth(int northSouth) {
		this.northSouth = northSouth;
	}

	public int getEastWest() {
		return eastWest;
	}

	public void setEastWest(int eastWest) {
		this.eastWest = eastWest;
	}

	public boolean isSouth(Coordinates compare) {
		return (compare.getNorthSouth() == (northSouth + 1) && compare.getEastWest() == eastWest);
	}

	public boolean isNorth(Coordinates compare) {
		return (compare.getNorthSouth() == (northSouth - 1) && compare.getEastWest() == eastWest);
	}

	public boolean isEast(Coordinates compare) {
		return (compare.getNorthSouth() == (northSouth) && compare.getEastWest() == eastWest - 1);
	}

	public boolean isWest(Coordinates compare) {
		return (compare.getNorthSouth() == (northSouth) && compare.getEastWest() == eastWest + 1);
	}

	public boolean isHere(Coordinates compare) {
		return (compare.getNorthSouth() == (northSouth) && compare.getEastWest() == eastWest);
	}

	@Override
	public String toString() {
		return "North-South grid location: " + northSouth + " East-West grid location: " + eastWest;
	}

	public void changeNorthSouth(int northSouthChange) {
		northSouth += northSouthChange;
	}

	public void changeEastWest(int eastWestChange) {
		eastWest += eastWestChange;

	}

}
