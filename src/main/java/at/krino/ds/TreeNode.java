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

import at.krino.lingo.LinguisticElement;
import java.util.HashMap;
import java.util.Map;

/**
 *
 * @author jan
 */
public class TreeNode extends Vertex {
    
    
    private boolean isRoot = false;
    private boolean isInternal = false;
    
    private TreeNode leftChild; 
    private TreeNode rightChild;
    
    private Map<LinguisticElement, String> payLoad = new HashMap<>();

    
    
    
    /**
     * @return the isRoot
     */
    public boolean isIsRoot() {
        return isRoot;
    }

    /**
     * @param isRoot the isRoot to set
     */
    public void setRoot(boolean isRoot) {
        this.isRoot = isRoot;
    }

    /**
     * @return the isInternal
     */
    public boolean isInternal() {
        return isInternal;
    }

    /**
     * @param isInternal the isInternal to set
     */
    public void setInternal(boolean isInternal) {
        this.isInternal = isInternal;
    }

    /**
     * @return the leftChild
     */
    public TreeNode getLeftChild() {
        return leftChild;
    }

    /**
     * @param leftChild the leftChild to set
     */
    public void setLeftChild(TreeNode leftChild) {
        this.leftChild = leftChild;
    }

    /**
     * @return the rightChild
     */
    public TreeNode getRightChild() {
        return rightChild;
    }

    /**
     * @param rightChild the rightChild to set
     */
    public void setRightChild(TreeNode rightChild) {
        this.rightChild = rightChild;
    }

    /**
     * @return the payLoad
     */
    public Map<LinguisticElement, String> getPayLoad() {
        return payLoad;
    }

    /**
     * @param payLoad the payLoad to set
     */
    public void setPayLoad(Map<LinguisticElement, String> payLoad) {
        this.payLoad = payLoad;
    }

}
