package at.krino;

import at.krino.ds.AdpTree;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.logging.Level;
import java.util.logging.Logger;


/**
 * @author Jan van Oort
 * @version 1.0
 * @created 30-Jan-2020 11:47:27 AM
 */
public class KrinoMain {

	

	public final static void main( String... args ) throws 
                ClassNotFoundException, 
                InstantiationException, 
                IllegalAccessException, 
                IllegalArgumentException, 
                InvocationTargetException   
        {
            
            KrinoConfig config = KrinoConfig.get();
            BlockingQueue< ? extends AdpTree > queue = new ArrayBlockingQueue<>( Integer.parseInt(config.get( "queue-size" )));
            Set< String > componentClasses = new HashSet<>();
            Set< KrinoComponent > components = new HashSet<>();
            
            componentClasses.add( config.get( "events") );
            componentClasses.add( config.get( "lingo") );
            componentClasses.add( config.get( "scenery") );
            componentClasses.add( config.get( "smg") );
            
            for( String className: componentClasses )   {
                Class<?> clazz = Class.forName(className);
                Constructor<?>[] c = clazz.getConstructors();
                for( Constructor cons: c )  {
                    if( cons.getParameterCount() == 1 && cons.getParameterTypes()[0].equals(BlockingQueue.class ) ){
                        KrinoComponent comp = ( KrinoComponent ) cons.newInstance( queue );
                        components.add( comp );
                        Thread t = new Thread( comp );
                        t.start();
                    }
                }
            }
            Thread tHook = new Thread( new KrinoShutdownHook(components) );
            Runtime.getRuntime().addShutdownHook(tHook);
	}

}
class KrinoShutdownHook implements Runnable     {

    private final Set< KrinoComponent > components;
    
    public KrinoShutdownHook( Set< KrinoComponent > comps) {
        components = comps;
    }

    
    
    
    @Override
    public void run() {
        components.forEach((c) -> {
            c.poison();
        });
        try {
            Thread.sleep( 1000 );
        } 
        catch (InterruptedException ex) {
            Thread.interrupted();
        }
        System.out.println( "Krino shut down, exiting. Goodbye cruel world." );
    }
    
}