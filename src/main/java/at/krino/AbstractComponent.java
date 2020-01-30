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

import at.krino.ds.AdpTree;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 *
 * @author Jan van Oort
 */
public abstract class AbstractComponent implements KrinoComponent {
    
    protected final BlockingQueue< AdpTree > queue;
    
    protected final AtomicBoolean poisonCookie = new AtomicBoolean( false );
    
    public AbstractComponent( BlockingQueue< AdpTree > q )  {
        queue = q;
    }
    
    
    public void poison()    {
        poisonCookie.set(true);
    }

}
