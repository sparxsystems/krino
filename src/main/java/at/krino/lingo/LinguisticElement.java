/*
 * Copyright Sparx Systems Central Europe GmbH, 2020.
 * All rights reserved.
 * krino is a trademark of Sparx Systems Central Europe, Austria.
 *
 * This artefact and the enclosing repository are governed by the MIT License. 
 * You should have received a copy of the license together with the repository's contents. 
 * In case you didn't, you can find it here: https://opensource.org/licenses/MIT
 */

package at.krino.lingo;

/**
 *
 * @author Jan van Oort
 */
public enum LinguisticElement {

    
    Morpheme( "Morpheme"), 
    Valency( "Valency"), 
    Saturation("Saturation"),
    Hook( "Hook"),
    Governor( "Governor"), 
    Dependent( "Dependent"); 
    
    private final String stringRep;
    
    private LinguisticElement( String s )  {
        stringRep = s;
    }
    
    @Override
    public String toString()    {
        return stringRep;
    }
}
