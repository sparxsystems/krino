using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions;
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


        public IPattern Pattern { get; set; } = new Pattern();

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
                    if (value != null && value.Right != this && value.Left != this)
                    {
                        throw new InvalidOperationException($"{nameof(IAdTree.AdPosition)} must be set via setting of {nameof(IAdTree.Right)} or {nameof(IAdTree.Left)}.");
                    }

                    myAdPosition = value;
                }
            }
        }

        public bool IsAdPosition => Right != null || Left != null;

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

        public IAdTree Right
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
                IAdTree rightChild = Right;
                while (rightChild != null)
                {
                    yield return rightChild;

                    rightChild = rightChild.Right;
                }
            }
        }

        public IAdTree Left
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

        public bool IsOnRight => AdPosition != null ? AdPosition.Right == this : false;

        public bool IsOnLeft => AdPosition != null ? AdPosition.Left == this : false;

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
                    result = governingAdPositions.Where(x => x.Left != null && x.Left.IsDependent);
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
                    result = Pattern.ValencyPosition > 0 ? this : AdPositions.FirstOrDefault(x => x.Pattern.ValencyPosition > 0);
                }
                else
                {
                    result = AdPositions.FirstOrDefault(x => x.Morpheme != null && x.Morpheme.Attributes.IsValencySpecified());
                }

                return result;
            }
        }

        public IEnumerable<IAdTree> ValencyAdPositions => IsGovernor ? AdPosition
            .TakeWhile(x => x.IsOnRight)
            .Where(x => x.Pattern.ValencyPosition > 0) : Enumerable.Empty<IAdTree>();


        public IEnumerable<IAdTree> Verbants
        {
            get
            {
                IEnumerable<IAdTree> result = this.Where(x => x.Morpheme.GrammarCharacter == GrammarCharacter.I);
                return result;
            }
        }

        public IEnumerable<IAdTree> PhraseElements
        {
            get
            {
                // If it is not an adposition.
                if (!IsAdPosition)
                {
                    yield return this;
                }
                else
                {
                    if (!Pattern.IsReversed)
                    {
                        if (Right != null)
                        {
                            foreach (IAdTree element in Right.PhraseElements)
                            {
                                yield return element;
                            }
                        }

                        yield return this;

                        if (Left != null)
                        {
                            foreach (IAdTree element in Left.PhraseElements)
                            {
                                yield return element;
                            }
                        }
                    }
                    else
                    {
                        if (Left != null)
                        {
                            foreach (IAdTree element in Left.PhraseElements)
                            {
                                yield return element;
                            }
                        }

                        yield return this;

                        if (Right != null)
                        {
                            foreach (IAdTree element in Right.PhraseElements)
                            {
                                yield return element;
                            }
                        }
                    }
                }
            }
        }

        public string Phrase => string.Join(" ", PhraseElements.Where(x => !string.IsNullOrEmpty(x.Morpheme?.Morph)).Select(x => x.Morpheme.Morph));


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

                if (aThis.Left != null)
                {
                    aStack.Push(aThis.Left);
                }

                if (aThis.Right != null)
                {
                    aStack.Push(aThis.Right);
                }
            }
        }


        

        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }
    }
}
