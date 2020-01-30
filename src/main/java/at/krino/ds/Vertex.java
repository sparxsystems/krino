/*
 * Copyright Sparx Systems Central Europe GmbH, 2020.
 * All rights reserved.
 * krino is a trademark of Sparx Systems Central Europe, Austria.
 *
 * This artefact and the enclosing repository are governed by the MIT License. 
 * You should have received a copy of the license together with the repository's contents. 
 * In case you didn't, you can find it here: https://opensource.org/licenses/MIT
 */

package at.krino.ds;

import java.util.HashSet;
import java.util.Set;

/**
 * This vertex class acts as a proxy for the vertex class of any graph database currently in use 
 * by krino. Although the original GoF Proxy design pattern works by inheritance, we can't use 
 * that mechanism here, as inheriting from an unknown class is impossible. 
 * 
 * @author Jan van Oort
 */
public class Vertex {

   
    protected final Set< Edge > edges = new HashSet<>(); 
    
    
    public void destroy()   {
        edges.forEach((e) -> {
            e.destroy();
        });
        edges.clear();
    }
}
