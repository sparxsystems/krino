using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Vertical.Utils.Collections;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees
{
    /// <summary>
    /// Implements the AdTree as defined in the document Constructive Adposition Grammar.
    /// </summary>
    public class AdTree : IAdTree
    {
        private IAdTree myAdPosition;
        private IAdTree myRightChild;
        private IAdTree myLeftChild;


        public IMorpheme Morpheme { get; set; }

        public GrammarCharacter GrammarCharacter => Morpheme?.GrammarCharacter ?? GrammarCharacter.Epsilon;

        public GrammarCharacter InheritedGrammarCharacter
        {
            get
            {
                GrammarCharacter result = GrammarCharacter.Epsilon;

                // Find the first element on the right branch which has defined own grammar character.
                IAdTree rightChild = RightChildren.FirstOrDefault(x => x.GrammarCharacter != GrammarCharacter.Epsilon);
                if (rightChild != null)
                {
                    result = rightChild.GrammarCharacter;
                }

                return result;
            }
        }


        public IAdTree AdPosition
        {
            get => myAdPosition;
            set
            {
                if (myAdPosition != value)
                {
                    if (value != null && value.RightChild != this && value.LeftChild != this)
                    {
                        throw new InvalidOperationException($"{nameof(IAdTree.AdPosition)} must be set via setting of {nameof(IAdTree.RightChild)} or {nameof(IAdTree.LeftChild)}.");
                    }

                    myAdPosition = value;
                }
            }
        }

        public bool IsAdPosition => RightChild != null || LeftChild != null;

        public IEnumerable<IAdTree> AdPositions
        {
            get
            {
                IAdTree parent = AdPosition;
                while (parent != null)
                {
                    yield return parent;

                    parent = parent.AdPosition;
                }
            }
        }

        public IAdTree RightChild
        {
            get => myRightChild;
            set
            {
                if (myRightChild != value)
                {
                    if (myRightChild != null)
                    {
                        myRightChild.AdPosition = null;
                    }

                    myRightChild = value;

                    if (myRightChild != null)
                    {
                        myRightChild.AdPosition = this;
                    }
                }
            }
        }

        public IEnumerable<IAdTree> RightChildren
        {
            get
            {
                IAdTree rightChild = RightChild;
                while (rightChild != null)
                {
                    yield return rightChild;

                    rightChild = rightChild.RightChild;
                }
            }
        }

        public IAdTree LeftChild
        {
            get => myLeftChild;
            set
            {
                if (myLeftChild != value)
                {
                    if (myLeftChild != null)
                    {
                        myLeftChild.AdPosition = null;
                    }

                    myLeftChild = value;

                    if (myLeftChild != null)
                    {
                        myLeftChild.AdPosition = this;
                    }
                }
            }
        }

        public bool IsOnRight => AdPosition != null ? AdPosition.RightChild == this : false;

        public bool IsOnLeft => AdPosition != null ? AdPosition.LeftChild == this : false;

        public IAdTree Governor
        {
            get
            {
                IAdTree result = null;

                // If this element is not a governor.
                if (!IsGovernor)
                {
                    // If this element is the root or is located on the right.
                    if (AdPosition == null || IsOnRight)
                    {
                        result = RightChildren.FirstOrDefault(x => x.GrammarCharacter != GrammarCharacter.Epsilon);
                    }
                    // This element is not the root and is located on the left.
                    else
                    {
                        // Go via all adpositions and find the first governor.
                        result = AdPositions.SelectMany(x => x.RightChildren)
                            .FirstOrDefault(x => x.GrammarCharacter != GrammarCharacter.Epsilon && x.GrammarCharacter != GrammarCharacter.U);
                    }
                }
                else
                {
                    // Find the first adposition which is located on the left or it is the root.
                    IAdTree adPositionOnLeftOrRoot = AdPositions.FirstOrDefault(x => x.IsOnLeft || x.AdPosition == null);

                    // Get the governor of the governor.
                    result = adPositionOnLeftOrRoot.Governor;
                }

                return result;
            }
        }

        public bool IsGovernor => IsOnRight && GrammarCharacter != GrammarCharacter.Epsilon && GrammarCharacter != GrammarCharacter.U;

        public IEnumerable<IAdTree> DependentAdPositions
        {
            get
            {
                IEnumerable<IAdTree> result;

                // Only governors have dependents.
                if (IsGovernor)
                {
                    IEnumerable<IAdTree> governingAdPositions = AdPositions.TakeUntil(x => x.AdPosition == null || x.IsOnRight && x.GrammarCharacter == GrammarCharacter.Epsilon);
                    result = governingAdPositions.Where(x => x.LeftChild != null && x.LeftChild.IsDependent);
                }
                else
                {
                    result = Enumerable.Empty<IAdTree>();
                }

                return result;
            }
        }

        // Note: dependent may have the epsilon grammar character.
        public bool IsDependent => IsOnLeft && GrammarCharacter != GrammarCharacter.U;

        public IAdTree ValencyAdPosition
        {
            get
            {
                IAdTree result = null;

                // If this element has not speciefied the valency attribute.
                if (Morpheme == null || !Morpheme.Attributes.IsValencySpecified())
                {
                    result = ValencyPosition > 0 ? this : AdPositions.FirstOrDefault(x => x.ValencyPosition > 0);
                }
                else
                {
                    result = AdPositions.FirstOrDefault(x => x.Morpheme != null && x.Morpheme.Attributes.IsValencySpecified());
                }

                return result;
            }
        }

        public int ValencyPosition { get; set; }


        public IEnumerable<IAdTree> ValencyAdPositions => IsGovernor ? AdPosition
            .TakeWhile(x => x.IsOnRight)
            .Where(x => x.ValencyPosition > 0) : Enumerable.Empty<IAdTree>();


        public IEnumerable<IAdTree> Verbants
        {
            get
            {
                IEnumerable<IAdTree> result = this.Where(x => x.Morpheme.GrammarCharacter == GrammarCharacter.I);
                return result;
            }
        }

        public IEnumerable<IAdTree> Phrase
        {
            get
            {
                // If it is not an adposition (if it is not a node).
                if (RightChild == null && LeftChild == null)
                {
                    yield return this;
                }
                // If it is a stative group.
                else if (InheritedGrammarCharacter == GrammarCharacter.O)
                {
                    //foreach (IAdTree adjunctive in Governors)
                }
                // If it is a verbant group.
                else if (InheritedGrammarCharacter == GrammarCharacter.I)
                {
                    // Try to find that verbant.
                    IAdTree verbant = Verbants.FirstOrDefault();
                    if (verbant != null)
                    {
                        // Try to find the stative on the first valency.
                        IAdTree stative1 = verbant.ValencyAdPositions.FirstOrDefault();
                        if (stative1 != null)
                        {
                            // Return phrase items of the stative1.
                            foreach (IAdTree stativePhraseItem in stative1.Phrase)
                            {
                                yield return stativePhraseItem;
                            }

                            // Return the adposition of the stative1.
                            yield return stative1.AdPosition;
                        }

                        // Try to find verbant modifiers (circumstantials).
                        IEnumerable<IAdTree> verbantModifiers = verbant.AdPositions.Where(x => x.LeftChild.Morpheme != null && x.LeftChild.Morpheme.GrammarCharacter == GrammarCharacter.E);
                        if (verbantModifiers != null)
                        {
                            // From top to down.
                            foreach (IAdTree modifier in verbantModifiers.Reverse())
                            {
                                // Return the modifier.
                                yield return modifier;

                                // Also return its adposition.
                                yield return modifier.AdPosition;
                            }
                        }

                        // Here comes the verbant.
                        yield return verbant;

                        // Continue up the 1st valency.
                        if (stative1.AdPosition.AdPositions != null)
                        {
                            foreach (IAdTree item in stative1.AdPosition.AdPositions)
                            {
                                // Return phrase items.
                                foreach (IAdTree phraseOfItem in item.Phrase)
                                {
                                    yield return phraseOfItem;
                                }

                                // Also return the adposition of the item.
                                yield return item.AdPosition;
                            }
                        }
                    }
                }
                // only statives, adjunctives and adpositions
                else
                {

                }


                //IEnumerable<IAdTree> result = this.
                // TODO:
                //return null;
            }
        }



        public IEnumerator<IAdTree> GetEnumerator()
        {
            // Note: using the stack has a better performance than a recursive call.
            //       Also there is not a danger of of the stack overflow.
            Stack<IAdTree> aStack = new Stack<IAdTree>();
            aStack.Push(this);

            while (aStack.Count > 0)
            {
                IAdTree aThis = aStack.Pop();
                yield return aThis;

                if (aThis.LeftChild != null)
                {
                    aStack.Push(aThis.LeftChild);
                }

                if (aThis.RightChild != null)
                {
                    aStack.Push(aThis.RightChild);
                }
            }
        }


        

        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }
    }
}
