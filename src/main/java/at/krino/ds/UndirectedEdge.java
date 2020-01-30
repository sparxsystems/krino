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

/**
 *
 * @author jan
 */
public final class UndirectedEdge extends Edge {

    
    
    public UndirectedEdge( Vertex u, Vertex v ) {
        super( false, u, v );
    }

    

}
