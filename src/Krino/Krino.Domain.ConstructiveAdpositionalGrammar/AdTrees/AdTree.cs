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

        // TODO:
        public RelationType Relation => RelationType.Generic;


        public IMorpheme Morpheme { get; set; }

        public GrammarCharacter RaisedGrammarCharacter
        {
            get
            {
                GrammarCharacter result = GrammarCharacter.None;

                IAdTree bottomGovernor = Governors.LastOrDefault();
                if (bottomGovernor != null)
                {
                    if (bottomGovernor.Morpheme != null)
                    {
                        result = bottomGovernor.Morpheme.Character;
                    }
                }
                // If it is not adposition then return own grammar character if exists.
                else if (Morpheme != null)
                {
                    result = Morpheme.Character;
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

        public IEnumerable<IAdTree> Governors
        {
            get
            {
                IAdTree governor = Governor;
                while (governor != null)
                {
                    yield return governor;

                    governor = governor.Governor;
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

        public IAdTree Sibling
        {
            get
            {
                IAdTree result;
                if (AdPosition != null)
                {
                    result = AdPosition.Governor == this ? AdPosition.Dependent : AdPosition.Governor;
                }
                else
                {
                    result = null;
                }

                return result;
            }
        }

        public int SaturatedValency
        {
            get
            {
                int saturatedValency = 0;

                // If this is a stative.
                if (AdPosition != null && RaisedGrammarCharacter == GrammarCharacter.O)
                {
                    // Try to find the parent adpostion which has the verbant as its raised grammar character.
                    IAdTree verbantAdPosition = AdPositions.FirstOrDefault(x => x.RaisedGrammarCharacter == GrammarCharacter.I);
                    if (verbantAdPosition != null)
                    {
                        // Count the distance between this stative and the verbant - that is the valency this stative saturates.
                        saturatedValency = verbantAdPosition.Governors
                            .Count(x => x.Dependent != null && x.Dependent.RaisedGrammarCharacter == GrammarCharacter.O || x.RaisedGrammarCharacter == GrammarCharacter.I);
                    }
                }

                return saturatedValency;
            }
        }

        public IReadOnlyList<IAdTree> SaturatedValencies
        {
            get
            {
                IReadOnlyList<IAdTree> result = null;

                // If this is a verbant.
                if (Morpheme != null && Morpheme.Character == GrammarCharacter.I)
                {
                    // Get stative dependents up to tree.
                    result = AdPositions.Where(x => x.Dependent.RaisedGrammarCharacter == GrammarCharacter.O)
                        .Select(x => x.Dependent)
                        .ToList();
                }

                return result;
            }
        }

        public IEnumerable<IAdTree> Verbants
        {
            get
            {
                IEnumerable<IAdTree> result = this.Where(x => x.Morpheme.Character == GrammarCharacter.I);
                return result;
            }
        }

        public IEnumerable<IAdTree> Phrase
        {
            get
            {
                //IEnumerable<IAdTree> result = this.
                // TODO:
                return null;
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
