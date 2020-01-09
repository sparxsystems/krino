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

import java.net.URL;
import java.util.HashMap;
import java.util.Map;
import org.apache.commons.configuration2.XMLConfiguration;
import org.apache.commons.configuration2.builder.fluent.Configurations;
import org.apache.commons.configuration2.ex.ConfigurationException;

/**
 * Central access point for the krino configuration. The configuration keys and values themselves can 
 * be found in this project's src/main/resources folder in an xml document, from where they are loaded 
 * by krino's ClassLoader. 
 * 
 * @author jan
 */
public class KrinoConfig {

    
    public final static String CONFIG_FILE="krino-config.xml";
    
    private XMLConfiguration localConfig;
    
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
        Configurations configs = new Configurations();
        try {
            ClassLoader classLoader = getClass().getClassLoader();
            final URL resource = classLoader.getResource( CONFIG_FILE );
            localConfig = configs.xml( resource );
            localConfig.setValidating( true ); 
            String db = localConfig.get( String.class, "graph-db" );
            configStore.put( "db", db );
        }
        catch( ConfigurationException ce )  {
            throw new DataException( ce );
        }
    }
    
    
    public String get( String key ) {
        return configStore.get( key );
    }
}
