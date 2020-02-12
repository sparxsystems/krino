package at.krino.lingo;

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
public class LingoMain extends AbstractComponent {

    
	public LingoMain(BlockingQueue< AdpTree > q, CyclicBarrier b){
            super( q, b );
	}

        
        
	public void run(){
            System.out.println( "running lingo (natural language parser)" );
            while( ! poisonCookie.get() )   {
                AdpTree t = queue.peek();
                if( t != null ) {
                    System.out.println( "lingo (natural language parser) is consuming an AdpTree. Miam, miam...");
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
            System.out.println( "lingo (natural language parser): stopped gracefully" );
	}


}
