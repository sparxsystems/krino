/*
 * Copyright Sparx Systems Central Europe GmbH, 2020.
 * All rights reserved.
 * krino is a trademark of Sparx Systems Central Europe, Austria.
 *
 * This artefact and the enclosing repository are governed by the MIT License. 
 * You should have received a copy of the license together with the repository's contents. 
 * In case you didn't, you can find it here: https://opensource.org/licenses/MIT
 */

package at.krino;

import java.io.File;
import java.io.FileInputStream;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

/**
 * Central access point for the krino configuration. The configuration keys and values themselves can 
 * be found in this project's src/main/resources folder in an xml document, from where they are loaded 
 * by krino's ClassLoader. 
 * 
 * @author Jan van Oort
 */
public class KrinoConfig {

    
    public final static String CONFIG_FILE="/data/netbeansprojects/sparx/krino/src/main/resources/krino.properties";
    
    
    private static KrinoConfig instance = null;
    
    private final Map<String, String> configStore = new HashMap<>();
    
    private KrinoConfig()   {
        init();
    }
    
    
    public final static KrinoConfig get()   {
        if( instance == null ) {
            instance = new KrinoConfig();
        }
        return instance;
    }
    
    
    private void init() {

        try {
            ClassLoader classLoader = getClass().getClassLoader();
            //final URL resource = classLoader.getResource( CONFIG_FILE );
            File propsFile = new File( CONFIG_FILE );
            Properties props = new Properties();
            props.load( new FileInputStream( propsFile ));
            configStore.put( "db", props.getProperty( "graph-db"));
            configStore.put( "events", props.getProperty( "events"));
            configStore.put( "lingo", props.getProperty( "lingo"));
            configStore.put( "scenery", props.getProperty( "scenery") );
            configStore.put( "smg", props.getProperty( "smg") );
            configStore.put( "queue-size", props.getProperty( "queue-size"));
        }
        catch( Exception e )  {
            throw new DataException( e );
        }
    }
    
    
    public String get( String key ) {
        return configStore.get( key );
    }
}
