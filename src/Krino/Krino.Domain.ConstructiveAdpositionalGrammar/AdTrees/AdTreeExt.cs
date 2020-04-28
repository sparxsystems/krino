using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees
{
    /// <summary>
    /// Extension helper methods for the adtree.
    /// </summary>
    public static class AdTreeExt
    {
        /// <summary>
        /// Returns true if the adTreeElement can be attached to the right banch of the adTree.
        /// </summary>
        /// <param name="adTree"></param>
        /// <param name="adTreeElement"></param>
        /// <returns></returns>
        public static bool CanAttachToRight(this IAdTree adTree, IAdTree adTreeElement)
        {
            // If there is a match from the adtree to the adtree element via the left branch.
            if (adTree.Pattern.RightRule.IsMatch(adTreeElement.Morpheme.Morph, adTreeElement.Morpheme.Attributes, adTreeElement.Pattern.PatternAttributes))
            {
                // If there is a match from the adtree element to the adtree via the adposition.
                if (adTreeElement.Pattern.AdPositionRule.IsMatch(adTree.Morpheme.Morph, adTree.Morpheme.Attributes, adTree.Pattern.PatternAttributes))
                {
                    return true;
                }
            }

            return false;
        }

        /// <summary>
        /// Returns true if the adTreeElement can be attached to the left banch of the adTree.
        /// </summary>
        /// <param name="adTree"></param>
        /// <param name="adTreeElement"></param>
        /// <returns></returns>
        public static bool CanAttachToLeft(this IAdTree adTree, IAdTree adTreeElement)
        {
            // If there is a match from the adtree to the adtree element via the left branch.
            if (adTree.Pattern.LeftRule.IsMatch(adTreeElement.Morpheme.Morph, adTreeElement.Morpheme.Attributes, adTreeElement.Pattern.PatternAttributes))
            {
                // If there is a match from the adtree element to the adtree via the adposition.
                if (adTreeElement.Pattern.AdPositionRule.IsMatch(adTree.Morpheme.Morph, adTree.Morpheme.Attributes, adTree.Pattern.PatternAttributes))
                {
                    return true;
                }
            }

            return false;
        }

    }
}
