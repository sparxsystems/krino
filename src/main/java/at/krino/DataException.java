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

/**
 *
 * @author Jan van Oort
 */
public class DataException extends RuntimeException {
    
    
    public DataException( Exception wrapped )   {
        super( wrapped );
    }
    
    
    public DataException( String msg )  {
        super( msg );
    }
            
}
