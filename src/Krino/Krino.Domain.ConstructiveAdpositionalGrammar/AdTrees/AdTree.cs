﻿using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.AttributesArrangement;
using Krino.Vertical.Utils.Collections;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Threading.Tasks;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees
{
    /// <summary>
    /// Implements the AdTree as defined in the document Constructive Adposition Grammar.
    /// </summary>
    [DebuggerDisplay("{Left?.Morpheme?.GrammarCharacter.ToString()}:{Left?.Morpheme?.Morph} <- {Morpheme?.GrammarCharacter.ToString()}:{Morpheme?.Morph} -> {Right?.Morpheme?.GrammarCharacter.ToString()}:{Right?.Morpheme?.Morph}")]
    public class AdTree : IAdTree
    {
        private IAdTree myAdPosition;
        private IAdTree myRightChild;
        private IAdTree myLeftChild;


        public AdTree(Morpheme morpheme, Pattern pattern)
        {
            Morpheme = morpheme ?? throw new ArgumentNullException(nameof(morpheme));
            Pattern = pattern ?? throw new ArgumentNullException(nameof(pattern));
        }

        public Pattern Pattern { get; }

        public Morpheme Morpheme { get; }

        public GrammarCharacter InheritedGrammarCharacter
        {
            get
            {
                GrammarCharacter result = GrammarCharacter.Epsilon;

                // Find the first element on the right branch which has defined own grammar character.
                IAdTree rightChild = RightChildren.FirstOrDefault(x => x.Morpheme.GrammarCharacter != GrammarCharacter.Epsilon);
                if (rightChild != null)
                {
                    result = rightChild.Morpheme.GrammarCharacter;
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
                        result = RightChildren.FirstOrDefault(x => x.Morpheme.GrammarCharacter != GrammarCharacter.Epsilon);
                    }
                    // This element is not the root and is located on the left.
                    else
                    {
                        // Go via all adpositions and find the first governor.
                        result = AdPositions.SelectMany(x => x.RightChildren)
                            .FirstOrDefault(x => x.Morpheme.GrammarCharacter != GrammarCharacter.Epsilon && x.Morpheme.GrammarCharacter != GrammarCharacter.U);
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

        public bool IsGovernor => IsOnRight && Morpheme.GrammarCharacter != GrammarCharacter.Epsilon && Morpheme.GrammarCharacter != GrammarCharacter.U;

        public IEnumerable<IAdTree> DependentAdPositions
        {
            get
            {
                IEnumerable<IAdTree> result;

                // Only governors have dependents.
                if (IsGovernor)
                {
                    IEnumerable<IAdTree> governingAdPositions = AdPositions.TakeUntil(x => x.AdPosition == null || x.IsOnRight && x.Morpheme.GrammarCharacter == GrammarCharacter.Epsilon);
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
        public bool IsDependent => IsOnLeft && Morpheme.GrammarCharacter != GrammarCharacter.U;

        public IAdTree ValencyAdPosition
        {
            get
            {
                IAdTree result = null;

                // If this element is left child.
                if (IsOnLeft)
                {
                    // then if this element saturates a valency position return it otherwise
                    // just iterate up and find the first adposition which saturates an adposition.
                    result = Pattern.MorphemeRule.ValencyPosition > 0 ? this : AdPositions.FirstOrDefault(x => x.Pattern.MorphemeRule.ValencyPosition > 0);
                }
                else if (IsOnRight)
                {
                    // Iterate up and find the first adposition which is on left.
                    IAdTree onLeftAdPosition = AdPositions.FirstOrDefault(x => x.IsOnLeft);
                    if (onLeftAdPosition != null)
                    {
                        result = onLeftAdPosition.ValencyAdPosition;
                    }
                }
                // This is the root.
                else
                {
                    if (Pattern.MorphemeRule.ValencyPosition > 0)
                    {
                        result = this;
                    }
                }

                return result;
            }
        }

        public IEnumerable<IAdTree> ValencyAdPositions => IsGovernor ? AdPosition
            .TakeWhile(x => x.IsOnRight)
            .Where(x => x.Pattern.MorphemeRule.ValencyPosition > 0) : Enumerable.Empty<IAdTree>();


        public async Task<IEnumerable<IAdTree>> GetPhraseElementsAsync()
        {
            // Collapse the call-stack and schedule the continuation to the queue.
            await Task.Yield();

            IEnumerable<IAdTree> result = Enumerable.Empty<IAdTree>();

            bool isLeftBeforeRight = Pattern.LeftRule.Order < Pattern.RightRule.Order;

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
                        if (!thisEnumerator.Current.Morpheme.Equals(otherEnumerator.Current.Morpheme) ||
                            !thisEnumerator.Current.Pattern.Equals(otherEnumerator.Current.Pattern))
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

        public override int GetHashCode()
        {
            int hash = 486187739;

            hash = (hash * 16777619) ^ Morpheme.GetHashCode();
            hash = (hash * 16777619) ^ Pattern.GetHashCode();

            return hash;
        }


        public IEnumerator<IAdTree> GetEnumerator()
        {
            // Note: using the stack has a better performance than a recursive call.
            //       Also there is not a danger of of the stack overflow.
            Stack<IAdTree> stack = new Stack<IAdTree>();
            stack.Push(this);

            while (stack.Count > 0)
            {
                IAdTree aThis = stack.Pop();
                yield return aThis;

                if (aThis.Left != null)
                {
                    stack.Push(aThis.Left);
                }

                if (aThis.Right != null)
                {
                    stack.Push(aThis.Right);
                }
            }
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }

        
    }
}
