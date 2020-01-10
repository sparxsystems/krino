/*
 * Copyright Sparx Systems Central Europe GmbH, 2020.
 * All rights reserved.
 * krino is a trademark of Sparx Systems Central Europe, Austria.
 *
 * This artefact and the enclosing repository are governed by the MIT License. 
 * You should have received a copy of the license together with the repository's contents. 
 * In case you didn't, you can find it here: https://opensource.org/licenses/MIT
 */

package at.krino.graph;

import java.util.Set;

/**
 *
 * @author Jan van Oort
 */
enum GraphDBType {
    
    
    ORIENTDB( OrientDBVertex.class, OrientDBEdge.class), 
    TITAN( null, null), 
    NEO4J( null, null), 
    ARANGODB( null, null );
    
    private final Class< ? extends Vertex> vertexClass;
    private final Class< ? extends Edge> edgeClass;
    
    private GraphDBType( Class< ? extends Vertex> vC,  Class< ? extends Edge > eC )       {
        vertexClass = vC;
        edgeClass = eC;
    }
    
    
    Class<? extends Vertex> getVertexClass()    {
        return vertexClass;
    }
    
    
    Class< ? extends Edge> getEdgeClass()   {
        return edgeClass;
    }

}

final class OrientDBEdge implements Edge  {

    
    @Override
    public boolean isDirected() {
        throw new UnsupportedOperationException("Not implemented yet.");
    }

    
    @Override
    public <V extends Vertex> V getTail() {
        throw new UnsupportedOperationException("Not implemented yet.");
    }

    
    @Override
    public <V extends Vertex> V getHead() {
        throw new UnsupportedOperationException("Not implemented yet.");
    }

    
    @Override
    public long getIDs() {
        throw new UnsupportedOperationException("Not implemented yet.");
    }
    
}


final class OrientDBVertex implements Vertex    {

    
    @Override
    public <E extends Edge> Set<E> getEdges() {
        throw new UnsupportedOperationException("Not implemented yet.");
    }

    
    @Override
    public <E extends Edge> Set<E> getInEdges() {
        throw new UnsupportedOperationException("Not implemented yet.");
    }

    
    @Override
    public <E extends Edge> Set<E> getOutEdges() {
        throw new UnsupportedOperationException("Not implemented yet.");
    }

    
    @Override
    public long[] getIDs() {
        throw new UnsupportedOperationException("Not implemented yet.");
    }
    
}
