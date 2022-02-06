using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes;
using Krino.Vertical.Utils.Collections;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees
{
    /// <summary>
    /// Implements the AdTree as defined in the document Constructive Adposition Grammar.
    /// </summary>
    [DebuggerDisplay("{DebuggerDisplay}")]
    public class AdTree : IAdTree
    {
        private IAdTree myAdPosition;
        private IAdTree myRightChild;
        private IAdTree myLeftChild;


        public AdTree(IMorpheme morpheme)
        {
            Morpheme = morpheme ?? throw new ArgumentNullException(nameof(morpheme));
        }

        public IMorpheme Morpheme { get; }

        public IAdTree AdPosition
        {
            get => myAdPosition;
            set
            {
                if (myAdPosition != value)
                {
                    // Note: only the parent can set the adposition property.
                    // If the parent adposition is not set.
                    if (value != null && value.Right != this && value.Left != this)
                    {
                        throw new InvalidOperationException($"{nameof(IAdTree.AdPosition)} must be first set by the parent via {nameof(IAdTree.Right)} or {nameof(IAdTree.Left)}.");
                    }
                    if (value == null && (myAdPosition.Right == this || myAdPosition.Left == this))
                    {
                        throw new InvalidOperationException($"{nameof(IAdTree.AdPosition)} must be first set to null by the parent via {nameof(IAdTree.Right)} or {nameof(IAdTree.Left)}.");
                    }

                    myAdPosition = value;
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
                    IAdTree previousRightChild = myRightChild;

                    myRightChild = value;

                    // Correctly unset the previous right child.
                    if (previousRightChild != null)
                    {
                        previousRightChild.AdPosition = null;
                    }

                    if (myRightChild != null)
                    {
                        myRightChild.AdPosition = this;
                    }
                }
            }
        }

        public IAdTree Left
        {
            get => myLeftChild;
            set
            {
                IAdTree previousLeftChild = myLeftChild;

                myLeftChild = value;

                // Correctly unset the previous left child.
                if (previousLeftChild != null)
                {
                    previousLeftChild.AdPosition = null;
                }

                if (myLeftChild != null)
                {
                    myLeftChild.AdPosition = this;
                }
            }
        }

        public bool IsLeftFirst { get; set; }

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

        public IAdTree Root => AdPosition == null ? this : AdPositions.Last();

        
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

        public bool IsOnRight => AdPosition != null ? AdPosition.Right == this : false;

        public bool IsOnLeft => AdPosition != null ? AdPosition.Left == this : false;

        public IAdTree GetMyGovernor()
        {
            IAdTree result = null;

            // If this element is not a governor.
            if (!IsGovernor)
            {
                // If this element is the root or is located on the right.
                if (AdPosition == null || IsOnRight)
                {
                    result = RightChildren.FirstOrDefault(x => x.IsGovernor);
                }
                // This element is not the root and is located on the left.
                else
                {
                    // Go via all adpositions and find the first governor.
                    result = AdPositions.SelectMany(x => x.RightChildren)
                        .FirstOrDefault(x => x.IsGovernor);
                }
            }
            else
            {
                // Find the first adposition which is located on the left or it is the root.
                IAdTree adPositionOnLeftOrRoot = AdPositions.FirstOrDefault(x => x.IsOnLeft || x.AdPosition == null);

                // Get the governor of the governor.
                result = adPositionOnLeftOrRoot.GetMyGovernor();
            }

            return result;
        }

        public bool IsGovernor => IsOnRight;

        public IEnumerable<IAdTree> DependentAdPositions
        {
            get
            {
                IEnumerable<IAdTree> result;

                // Only governors have dependents.
                if (IsGovernor)
                {
                    IEnumerable<IAdTree> governingAdPositions = AdPositions.TakeUntil(x => x.AdPosition == null || x.IsOnRight && x.Morpheme.GrammarCharacter == GrammarCharacter.e);
                    result = governingAdPositions.Where(x => x.Left != null && x.Left.IsDependent);
                }
                else
                {
                    result = Enumerable.Empty<IAdTree>();
                }

                return result;
            }
        }

        // Note: dependent may have the epsilon grammar character. E.g. if it is a group of dependents.
        public bool IsDependent => IsOnLeft && Morpheme.GrammarCharacter != GrammarCharacter.U;

        //public string Phrase2 => string.Join(" ", this.Where(x => !string.IsNullOrEmpty(x.Morpheme?.Morph)).Select(x => x.Morpheme.Morph));

        public string Phrase
        {
            get
            {
                var result = new StringBuilder();

                bool isFirst = true;
                var items = this.Where(x => !string.IsNullOrEmpty(x.Morpheme?.Value));
                foreach (var item in items)
                {
                    if (isFirst)
                    {
                        result.Append(item.Morpheme.Value);
                        isFirst = false;
                    }
                    else
                    {
                        if (!GrammarAttributes.Morpheme.IsSuffix(item.Morpheme.Attributes))
                        {
                            result.Append(" ");
                        }

                        result.Append(item.Morpheme.Value);
                    }
                }

                return result.ToString();
            }
        }

        public bool Equals(IAdTree other)
        {
            // Note: to avoid the recursion use the enumeration.

            using (IEnumerator<IAdTree> thisEnumerator = GetEnumerator())
            {
                using (IEnumerator<IAdTree> otherEnumerator = other.GetEnumerator())
                {
                    bool thisFlag = false;
                    bool otherFlag = false;

                    Func<bool> moveNext = () =>
                    {
                        thisFlag = thisEnumerator.MoveNext();
                        otherFlag = otherEnumerator.MoveNext();
                        return thisFlag && otherFlag;
                    };

                    while (moveNext())
                    {
                        if (thisEnumerator.Current.Morpheme.Value != otherEnumerator.Current.Morpheme.Value ||
                            thisEnumerator.Current.Morpheme.Attributes != otherEnumerator.Current.Morpheme.Attributes)
                        {
                            return false;
                        }
                    }

                    if (thisFlag != otherFlag)
                    {
                        return false;
                    }

                    return true;
                }
            }
        }

        public override int GetHashCode() => Morpheme.GetHashCode();
        


        public IEnumerable<IAdTree> GetAdTreesInAdTreeOrder()
        {
            var enumerator = GetEnumeratorInternal(false);
            while (enumerator.MoveNext())
            {
                yield return enumerator.Current;
            }
        }

        public IEnumerator<IAdTree> GetEnumerator() => GetEnumeratorInternal(true);
        

        private IEnumerator<IAdTree> GetEnumeratorInternal(bool phraseOrder)
        {
            // Note: using the stack has a better performance than a recursive call.
            //       Also there is not a danger of the stack overflow.
            // Note: the enumeration order matches the adtree order.
            Stack<IAdTree> stack = new Stack<IAdTree>();
            stack.Push(this);

            Stack<IAdTree> adPositionStack = new Stack<IAdTree>();

            while (stack.Count > 0)
            {
                IAdTree aThis = stack.Pop();

                if (aThis != null && (aThis.Left != null || aThis.Right != null))
                {
                    adPositionStack.Push(aThis);

                    // If left is before right.
                    if (phraseOrder && aThis.IsLeftFirst)
                    {
                        stack.Push(aThis.Right);
                        stack.Push(aThis.Left);
                    }
                    else
                    {
                        stack.Push(aThis.Left);
                        stack.Push(aThis.Right);
                    }
                }
                else
                {
                    if (aThis != null)
                    {
                        yield return aThis;
                    }

                    if (adPositionStack.Count > 0)
                    {
                        IAdTree adPosition = adPositionStack.Pop();
                        yield return adPosition;
                    }
                }
            }
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }

        private string DebuggerDisplay => Phrase;
        
    }
}
