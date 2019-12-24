package aoc.days.day23;

public class NAT implements Runnable {
    private Packet lastReceivedPacket;
    private Packet lastSentPacket;
    private final NetworkCommunicator cm0;
    private final Network network;

    public NAT(NetworkCommunicator cm0, Network network) {
        this.cm0 = cm0;
        this.network = network;
    }

    @Override
    public void run() {
        while (true) {
            synchronized (this) {
                while (lastReceivedPacket == null) {
                    try {
                        wait();
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                    }
                }
            }
            if (network.isIdle()) {
                cm0.makeRequest(lastReceivedPacket);
                synchronized (this) {
                    if (lastSentPacket != null && lastSentPacket.getY() == lastReceivedPacket.getY()) {
                        System.out.println(lastReceivedPacket.getY());
                        System.exit(0);
                    }
                    lastSentPacket = lastReceivedPacket;
                    lastReceivedPacket = null;
                }
            }
        }
    }

    public void receivePacket(Packet packet) {
        synchronized (this) {
            lastReceivedPacket = packet;
            lastReceivedPacket.setSender(255);
            notifyAll();
        }
    }
}
