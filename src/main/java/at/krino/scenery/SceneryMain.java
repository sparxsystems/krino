package at.krino.scenery;

import at.krino.AbstractComponent;
import at.krino.ds.AdpTree;

import java.util.concurrent.BlockingQueue;




/**
 * @author Jan van Oort
 * @version 1.0
 * @created 30-Jan-2020 11:47:27 AM
 */
public class SceneryMain extends AbstractComponent {

	public SceneryMain(BlockingQueue< AdpTree > q){
            super( q );
	}

        
	public void run(){
            System.out.println( "running Scenery (semantic context)" );
            while( ! poisonCookie.get() )   {
                AdpTree t = queue.peek();
            }
            System.out.println( "Scenery (semantic context): stopped gracefully" );
	}


}