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
        private IAdTree myGovernor;
        private IAdTree myDependent;

        public AdTreeElementType ElementType
        {
            get
            {
                AdTreeElementType result;
                if (AdPosition != null)
                {
                    // If the parent has this AdTree as the governor.
                    if (AdPosition.Governor == this)
                    {
                        result = AdTreeElementType.Governor;
                    }
                    // If the parent has this AdTree as the dependent.
                    else if (AdPosition.Dependent == this)
                    {
                        result = AdTreeElementType.Dependent;
                    }
                    else
                    {
                        throw new InvalidOperationException("The sub-tree has the parent but it is neither governor nor dependent.");
                    }

                    // If this AdTree has a child governor or dependent then it is also the AdPosition.
                    if (Governor != null || Dependent != null)
                    {
                        result |= AdTreeElementType.AdPosition;
                    }
                }
                // If this AdTree is the root then it the AdPostion.
                else
                {
                    result = AdTreeElementType.AdPosition;
                }

                return result;
            }
        }

        public Morpheme Morpheme { get; set; }

        public Prominence InformationProminence { get; set; }

        public IAdTree AdPosition
        {
            get => myAdPosition;
            set
            {
                if (myAdPosition != value)
                {
                    if (value != null && value.Governor != this && value.Dependent != this)
                    {
                        throw new InvalidOperationException($"{nameof(IAdTree.AdPosition)} must be set via setting of {nameof(IAdTree.Governor)} or {nameof(IAdTree.Dependent)}.");
                    }

                    myAdPosition = value;
                }
            }
        }

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

        public IAdTree Governor
        {
            get => myGovernor;
            set
            {
                if (myGovernor != value)
                {
                    if (myGovernor != null)
                    {
                        myGovernor.AdPosition = null;
                    }

                    myGovernor = value;

                    if (myGovernor != null)
                    {
                        myGovernor.AdPosition = this;
                    }
                }
            }
        }

        public IAdTree Dependent
        {
            get => myDependent;
            set
            {
                if (myDependent != value)
                {
                    if (myDependent != null)
                    {
                        myDependent.AdPosition = null;
                    }

                    myDependent = value;

                    if (myDependent != null)
                    {
                        myDependent.AdPosition = this;
                    }
                }
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

                if (aThis.Dependent != null)
                {
                    aStack.Push(aThis.Dependent);
                }

                if (aThis.Governor != null)
                {
                    aStack.Push(aThis.Governor);
                }
            }
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }
    }
}
