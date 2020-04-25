using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Vertical.Utils.Collections;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Parser.Builder
{
    public class AdTreeBuilder
    {
        private IEnumerable<IPattern> myPatterns;
        private List<IAdTree> myActiveAdTrees = new List<IAdTree>();

        private ITree<IPattern> myConstructions;

        public AdTreeBuilder(IEnumerable<IPattern> patterns)
        {
            myPatterns = patterns;
        }

        public bool AddPattern(IMorpheme morpheme)
        {
            bool isConnected = false;

            IEnumerable<IPattern> morphemeMatchingPatterns = GetApplicablePatterns(morpheme);

            // Update active adtrees.
            if (myActiveAdTrees.Count != 0)
            {
                foreach (IAdTree adTree in myActiveAdTrees)
                {
                    //if (IsMatch(adTree.Pattern, ))
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
            }


            return isConnected;
        }

        public IEnumerable<IAdTree> ToAdTrees()
        {
            return null;
        }

        private IEnumerable<IPattern> GetApplicablePatterns(IMorpheme morpheme)
        {
            foreach (IPattern pattern in myPatterns)
            {
                if (IsMatch(pattern, morpheme))
                {
                    yield return pattern;
                }
            }
        }

        public bool IsMatch(IPattern pattern, IMorpheme morpheme)
        {
            bool isMatch = true;

            // Check if it matches the required morpheme.
            if (pattern.MorphemeMatchingRule != null)
            {
                if (!string.IsNullOrEmpty(pattern.MorphemeMatchingRule.AcceptedMorph))
                {
                    if (pattern.MorphemeMatchingRule.AcceptedMorph != morpheme.Morph)
                    {
                        isMatch = false;
                    }
                }

                if (isMatch)
                {
                    ulong attributesCheckResult = pattern.MorphemeMatchingRule.AcceptedAttributes & morpheme.Attributes;
                    if (attributesCheckResult != pattern.MorphemeMatchingRule.AcceptedAttributes)
                    {
                        isMatch = false;
                    }
                }


                // Check if it matches with a not accepted morpheme.
                if (!string.IsNullOrEmpty(pattern.MorphemeMatchingRule.RejectedMorph))
                {
                    if (pattern.MorphemeMatchingRule.RejectedMorph == morpheme.Morph)
                    {
                        isMatch = false;
                    }
                }

                if (isMatch)
                {
                    ulong attributesCheckResult = pattern.MorphemeMatchingRule.RejectedAttributes & morpheme.Attributes;
                    if (attributesCheckResult == pattern.MorphemeMatchingRule.RejectedAttributes)
                    {
                        isMatch = false;
                    }
                }
            }

            

            return isMatch;
        }

        public bool IsMatch(IAdTree attachedAdTree)
        {
            bool isMatch = false;

            if (attachedAdTree.IsAdPosition)
            {
                // If it is attached via the left child.
                if (attachedAdTree.Left != null)
                {
                    // Check left-adposition connection.
                }
                else if (attachedAdTree.Right != null)
                {
                    // Check right-adposition connection.
                }
            }
            else if (attachedAdTree.IsOnLeft)
            {
                // Check adposition-left connection.

                if (attachedAdTree.IsDependent)
                {
                    // Check the governor too.
                }
            }
            else if (attachedAdTree.IsOnRight)
            {
                // Check adposition-right connection.
            }

            return isMatch;
        }

        
    }
}
