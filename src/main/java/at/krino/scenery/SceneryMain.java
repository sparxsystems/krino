package at.krino.scenery;

import at.krino.AbstractComponent;
import at.krino.ds.AdpTree;

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.BrokenBarrierException;
import java.util.concurrent.CyclicBarrier;




/**
 * @author Jan van Oort
 * @version 1.0
 * @created 30-Jan-2020 11:47:27 AM
 */
public class SceneryMain extends AbstractComponent {

	public SceneryMain(BlockingQueue< AdpTree > q, CyclicBarrier b){
            super( q, b );
	}

        
	public void run(){
            System.out.println( "running Scenery (semantic context)" );
            while( ! poisonCookie.get() )   {
                AdpTree t = queue.peek();
                if( t != null ) {
                    System.out.println( "Scenery (semantic context) is consuming an AdpTree. Chomp, chomp...");
                }
                else continue;
                try {
                    synch.await();
                } 
                catch (InterruptedException ex) {
                    Thread.interrupted();
                } 
                catch (BrokenBarrierException ex) {
                    return;
                }
            }
            System.out.println( "Scenery (semantic context): stopped gracefully" );
	}


}