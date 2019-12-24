package aoc.days.day23;

import aoc.utils.intcode.Controller;
import aoc.utils.intcode.IntcodeComputer;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.Map;
import java.util.Objects;

public class NetworkCommunicator implements Runnable, Controller {
    private static Map<Long, NetworkCommunicator> addressMap;
    private static NAT nat;
    private static boolean debug = true;

    private long[] memory;
    private LinkedList<Packet> requests;
    private Packet currentReceivedPacket;
    private PacketBuilder currentPackageToSend;
    private boolean isIdle;
    private long id;

    public NetworkCommunicator(long[] memory, long initialValue) {
        isIdle = false;
        id = initialValue;
        Packet initial = new Packet(0,0, 0, initialValue); //Fulhack
        initial.getNext();
        currentReceivedPacket = initial;
        this.memory = memory;
        requests = new LinkedList<>();
        currentPackageToSend = new PacketBuilder(id);
    }

    @Override
    public long getInput() {
        synchronized (this) {
            if (currentReceivedPacket.isEmpty() && requests.isEmpty()) {
                isIdle = true;
                return -1;
            }
            isIdle = false;
            if (currentReceivedPacket.isEmpty()) {
                currentReceivedPacket = requests.getFirst();
                requests.removeFirst();
                if (debug) {
                    System.out.println(id + " received packet " + currentReceivedPacket.toString() + " from " + currentReceivedPacket.getSender());
                }
            }
            return currentReceivedPacket.getNext();
        }
    }

    @Override
    public void output(long val) {
        synchronized (this) {
            currentPackageToSend.setNext(val);
            if (currentPackageToSend.isDone()) {
                Packet toSend = currentPackageToSend.build();
                long address = toSend.getAddress();
                if (address == 255) {
                    nat.receivePacket(toSend);
                    if (debug) {
                        System.out.println(id + " sent packet " + toSend.toString() + " to nat");
                    }
                } else {
                    addressMap.get(address).makeRequest(toSend);
                    if (debug) {
                        System.out.println(id + " sent packet " + toSend.toString() + " to " + address);
                    }
                }
                currentPackageToSend = new PacketBuilder(id);
            }
        }
    }

    public void makeRequest(Packet packet) {
        synchronized (this) {
            requests.addLast(packet);
        }
    }

    @Override
    public void run() {
        new IntcodeComputer(this, memory).execute();
        System.out.println(id + " halted");
    }

    public static void setAddressMap(Map<Long, NetworkCommunicator> addressMap) {
        NetworkCommunicator.addressMap = addressMap;
    }

    public static void setNat(NAT nat) {
        NetworkCommunicator.nat = nat;
    }

    public boolean isIdle() {
        synchronized (this) {
            return isIdle;
        }
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        NetworkCommunicator that = (NetworkCommunicator) o;
        return isIdle == that.isIdle &&
                id == that.id &&
                Arrays.equals(memory, that.memory) &&
                Objects.equals(requests, that.requests) &&
                Objects.equals(currentReceivedPacket, that.currentReceivedPacket) &&
                Objects.equals(currentPackageToSend, that.currentPackageToSend);
    }

    @Override
    public int hashCode() {
        synchronized (this) {
            int result = Objects.hash(requests, currentReceivedPacket, currentPackageToSend, isIdle, id);
            result = 31 * result + Arrays.hashCode(memory);
            return result;
        }
    }
}
