package at.krino.events;

import at.krino.AbstractComponent;
import at.krino.ds.AdpTree;

import java.util.concurrent.BlockingQueue;



/**
 * @author Jan van Oort
 * @version 1.0
 * @created 30-Jan-2020 11:47:27 AM
 */
public class EventsMain extends AbstractComponent {

    
	public EventsMain( BlockingQueue< AdpTree > q ) {
            super( q );
	}

        
        @Override
	public void run(){
            System.out.println( "running event processing (Truth Maintenance System)");
            while( ! poisonCookie.get() )   {
                AdpTree t = queue.peek();
            }
            System.out.println( "event processing (Truth Maintenance System): stopped gracefully" );
	}


}