using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.StructuralAttributesArrangement;
using System.Collections.Generic;
using System.Linq;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Parsing
{
    public class AdTreeBuilder
    {
        private IEnumerable<IPattern> myPatterns;
        private List<IAdTree> myActiveAdTrees = new List<IAdTree>();

        public AdTreeBuilder(IEnumerable<IPattern> patterns)
        {
            myPatterns = patterns;
        }

        public IReadOnlyList<IAdTree> ActiveAdTrees => myActiveAdTrees;

        public bool AddMorpheme(IMorpheme morpheme)
        {
            bool isAdded = false;

            IEnumerable<IPattern> morphemeMatchingPatterns = GetApplicablePatterns(morpheme);

            // Update active adtrees.
            if (myActiveAdTrees.Count != 0)
            {
                List<IAdTree> newListOfAdTrees = new List<IAdTree>();

                // Go via active adtrees (i.e. adtrees which are still candidates to become the final adtree).
                foreach(IAdTree currentAdTree in myActiveAdTrees)
                {
                    // Go via patterns which match the incoming morpheme.
                    foreach (IPattern pattern in morphemeMatchingPatterns)
                    {
                        IAdTree newAdTreeElement = new AdTree()
                        {
                            Morpheme = morpheme,
                            Pattern = pattern,
                        };

                        AttachAdTreeElement(currentAdTree, newAdTreeElement, newListOfAdTrees);
                    }
                }

                if (newListOfAdTrees.Count > 0)
                {
                    myActiveAdTrees = newListOfAdTrees;
                    isAdded = true;
                }
            }
            // Create new adtrees.
            else
            {
                foreach (IPattern pattern in morphemeMatchingPatterns)
                {
                    AdTree newAdTree = new AdTree()
                    {
                        Morpheme = morpheme,
                        Pattern = pattern
                    };

                    myActiveAdTrees.Add(newAdTree);
                }

                if (myActiveAdTrees.Count > 0)
                {
                    isAdded = true;
                }
            }


            return isAdded;
        }


        private IEnumerable<IPattern> GetApplicablePatterns(IMorpheme morpheme)
        {
            foreach (IPattern pattern in myPatterns)
            {
                if (pattern.MorphemeRule.IsMatch(morpheme.Morph, morpheme.Attributes))
                {
                    yield return pattern;
                }
            }
        }

        private void AttachAdTreeElement(IAdTree adTree, IAdTree newAdTreeElement, List<IAdTree> adTrees)
        {
            // If the new adtree element is an adposition then try to attach the adtree to its left ot right branch.
            if (newAdTreeElement.Pattern.LeftRule != PatternRule.Nothing || newAdTreeElement.Pattern.LeftRule != PatternRule.Nothing)
            {
                // If the new adtree element is an adposition then try to attach adtree to the left.
                if (newAdTreeElement.Pattern.LeftRule != PatternRule.Nothing)
                {
                    if (newAdTreeElement.CanAttachToLeft(adTree))
                    {
                        IAdTree copy = adTree.MakeShallowCopy();
                        newAdTreeElement.Left = adTree;
                        adTrees.Add(newAdTreeElement);
                    }
                }

                // If the new adtree element is an adposition then try to attach adtree to the right.
                if (newAdTreeElement.Pattern.LeftRule != PatternRule.Nothing)
                {
                    if (newAdTreeElement.CanAttachToRight(adTree))
                    {
                        IAdTree copy = adTree.MakeShallowCopy();
                        newAdTreeElement.Right = adTree;
                        adTrees.Add(newAdTreeElement);
                    }
                }
            }
            // If the new adtree element is not the adposition.
            else
            {
                // If the adtree is an adposition then try to attach the new element to its left or right branch.
                if (adTree.Pattern.LeftRule != PatternRule.Nothing || adTree.Pattern.RightRule != PatternRule.Nothing)
                {
                    // If the adtree is an adposition which does not have attached a left child.
                    if (adTree.Pattern.LeftRule != PatternRule.Nothing && adTree.Left == null)
                    {
                        if (adTree.CanAttachToLeft(newAdTreeElement))
                        {
                            IAdTree copy = adTree.MakeShallowCopy();
                            copy.Left = newAdTreeElement;
                            adTrees.Add(newAdTreeElement);
                        }
                    }

                    // If the adtree is an adposition which does not have attached a right child.
                    if (adTree.Pattern.RightRule != PatternRule.Nothing && adTree.Right == null)
                    {
                        if (adTree.CanAttachToRight(newAdTreeElement))
                        {
                            IAdTree copy = adTree.MakeShallowCopy();
                            copy.Right = newAdTreeElement;
                            adTrees.Add(newAdTreeElement);
                        }
                    }
                }
                // adtree as well as the new element are not adpositions so try to connect them via a matching adposition.
                else
                {
                    foreach (IPattern pattern in myPatterns)
                    {
                        if (pattern.CanAttachToLeft(adTree) && pattern.CanAttachToRight(newAdTreeElement))
                        {
                            IAdTree copy = adTree.MakeShallowCopy();

                            IAdTree newAdPosition = new AdTree() { Morpheme = new Morpheme(pattern.MorphemeRule.RequiredMorph) { Attributes = pattern.MorphemeRule.RequiredAttributes } };
                            newAdPosition.Right = newAdTreeElement;
                            newAdPosition.Left = copy;

                            adTrees.Add(newAdPosition);
                        }

                        if (pattern.CanAttachToLeft(newAdTreeElement) && pattern.CanAttachToRight(adTree))
                        {
                            IAdTree copy = adTree.MakeShallowCopy();

                            IAdTree newAdPosition = new AdTree() { Morpheme = new Morpheme(pattern.MorphemeRule.RequiredMorph) { Attributes = pattern.MorphemeRule.RequiredAttributes } };
                            newAdPosition.Right = copy;
                            newAdPosition.Left = newAdTreeElement;

                            adTrees.Add(newAdPosition);
                        }
                    }
                }
            }
        }
    }
}
