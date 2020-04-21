using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.StructuralAttributes;
using Krino.Vertical.Utils.Collections;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

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

        public GrammarCharacterType GrammarCharacter => Morpheme?.GrammarCharacter ?? GrammarCharacterType.Epsilon;

        public GrammarCharacterType InheritedGrammarCharacter
        {
            get
            {
                GrammarCharacterType result = GrammarCharacterType.Epsilon;

                // Find the first element on the right branch which has defined own grammar character.
                IAdTree rightChild = RightChildren.FirstOrDefault(x => x.GrammarCharacter != GrammarCharacterType.Epsilon);
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
                        result = RightChildren.FirstOrDefault(x => x.GrammarCharacter != GrammarCharacterType.Epsilon);
                    }
                    // This element is not the root and is located on the left.
                    else
                    {
                        // Go via all adpositions and find the first governor.
                        result = AdPositions.SelectMany(x => x.RightChildren)
                            .FirstOrDefault(x => x.GrammarCharacter != GrammarCharacterType.Epsilon && x.GrammarCharacter != GrammarCharacterType.U);
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

        public bool IsGovernor => IsOnRight && GrammarCharacter != GrammarCharacterType.Epsilon && GrammarCharacter != GrammarCharacterType.U;

        public IEnumerable<IAdTree> DependentAdPositions
        {
            get
            {
                IEnumerable<IAdTree> result;

                // Only governors have dependents.
                if (IsGovernor)
                {
                    IEnumerable<IAdTree> governingAdPositions = AdPositions.TakeUntil(x => x.AdPosition == null || x.IsOnRight && x.GrammarCharacter == GrammarCharacterType.Epsilon);
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
        public bool IsDependent => IsOnLeft && GrammarCharacter != GrammarCharacterType.U;

        public IAdTree ValencyAdPosition
        {
            get
            {
                IAdTree result = null;

                // If this element does not have specified any valency attribute.
                if (Morpheme == null || !StructAttributes.Verb.IsValencySpecified(Morpheme.Attributes))
                {
                    result = Pattern.ValencyPosition > 0 ? this : AdPositions.FirstOrDefault(x => x.Pattern.ValencyPosition > 0);
                }
                else
                {
                    result = AdPositions.FirstOrDefault(x => x.Morpheme != null && StructAttributes.Verb.IsValencySpecified(Morpheme.Attributes));
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
                IEnumerable<IAdTree> result = this.Where(x => x.Morpheme.GrammarCharacter == GrammarCharacterType.I);
                return result;
            }
        }


        public async Task<IEnumerable<IAdTree>> GetPhraseElementsAsync()
        {
            // Collapse the call-stack and schedule the continuation to the queue.
            await Task.Yield();

            IEnumerable<IAdTree> result = Enumerable.Empty<IAdTree>();

            bool isLeftBeforeRight = IsLeftBeforeRight();

            IAdTree first =  isLeftBeforeRight ? Left : Right;
            IAdTree second = this;
            IAdTree third = isLeftBeforeRight ? Right : Left;

            if (first != null)
            {
                IEnumerable<IAdTree> subFraseElements = await first.GetPhraseElementsAsync();
                result = result.Concat(subFraseElements);
            }

            result = result.Concat(new[] { second });

            if (third != null)
            {
                IEnumerable<IAdTree> subFraseElements = await third.GetPhraseElementsAsync();
                result = result.Concat(subFraseElements);
            }

            return result;
        }

        public string Phrase => string.Join(" ", GetPhraseElementsAsync().Result.Where(x => !string.IsNullOrEmpty(x.Morpheme?.Morph)).Select(x => x.Morpheme.Morph));


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

        private bool IsLeftBeforeRight()
        {
            bool isLeftBeforeRight = false;

            // If this is an adposition wich specifies the left child.
            if (Left != null)
            {
                // If this is a structural adposition.
                if (GrammarCharacter == GrammarCharacterType.Epsilon)
                {
                    // If this adposition specifies the first valency.
                    if (Pattern != null && Pattern.ValencyPosition == 1)
                    {
                        isLeftBeforeRight = true;
                    }
                    else if (Left.Morpheme != null)
                    {
                        if (StructAttributes.Adjective.Attributive.IsIn(Left.Morpheme.Attributes) ||
                            StructAttributes.Determiner.IsIn(Left.Morpheme.Attributes) ||
                            StructAttributes.Verb.Modal.IsIn(Left.Morpheme.Attributes) ||
                            StructAttributes.Preposition.IsIn(Left.Morpheme.Attributes) ||
                            StructAttributes.Numeral.IsIn(Left.Morpheme.Attributes)
                            )
                        {
                            isLeftBeforeRight = true;
                        }
                    }
                }
            }

            return isLeftBeforeRight;
        }
        

        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }
    }
}
