
public interface PlayerSubject {

	public void registerObserver(PlayerObserver observerToRegister);

	public void removeObserver(PlayerObserver observerToRemove);

	public void notifyObservers();

}
