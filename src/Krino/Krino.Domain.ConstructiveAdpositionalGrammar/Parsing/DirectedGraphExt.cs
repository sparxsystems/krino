using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Vertical.Utils.Graphs;
using System.Collections.Generic;
using System.Linq;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Parsing
{
    internal static class DirectedGraphExt
    {
        public static IEnumerable<IAdTree> GetPossibleAdTrees(this DirectedGraph<Pattern, AdTreePosition> patternGraph, Pattern start, IAttributesModel attributesModel, int maxMorphemes)
        {
            if (start.IsLikeMorpheme)
            {
                var adTree = new AdTree(Morpheme.Epsilon(attributesModel), start);
                yield return adTree;
            }
            else
            {
                var rightChildPatterns = patternGraph.GetEdgesGoingFrom(start)
                .Where(x => x.Value == AdTreePosition.ParrentForChildOnRight)
                .Select(x => x.To);

                var leftChildPatterns = patternGraph.GetEdgesGoingFrom(start)
                    .Where(x => x.Value == AdTreePosition.ParrentForChildOnRight)
                    .Select(x => x.To);

                var rightSubAdTrees = rightChildPatterns
                    .SelectMany(x => GetPossibleAdTrees(patternGraph, x, attributesModel, maxMorphemes))
                    .ToList();

                var leftSubAdTrees = leftChildPatterns
                    .SelectMany(x => GetPossibleAdTrees(patternGraph, x, attributesModel, maxMorphemes))
                    .ToList();

                if (rightSubAdTrees.Any() && leftSubAdTrees.Any())
                {
                    foreach (var rightSubAdTree in rightSubAdTrees)
                    {
                        foreach (var leftSubAdTree in leftSubAdTrees)
                        {
                            var adTree = new AdTree(Morpheme.Epsilon(attributesModel), start);
                            adTree.Right = rightSubAdTree;
                            adTree.Left = leftSubAdTree;

                            var morphemesCount = GetMorphemesCount(adTree);
                            if (morphemesCount <= maxMorphemes)
                            {
                                yield return adTree;
                            }
                        }
                    }
                }
                else if (rightSubAdTrees.Any())
                {
                    foreach (var rightSubAdTree in rightSubAdTrees)
                    {
                        var adTree = new AdTree(Morpheme.Epsilon(attributesModel), start);
                        adTree.Right = rightSubAdTree;

                        var morphemesCount = GetMorphemesCount(adTree);
                        if (morphemesCount <= maxMorphemes)
                        {
                            yield return adTree;
                        }
                    }
                }
                else if (leftSubAdTrees.Any())
                {
                    foreach (var leftSubAdTree in leftSubAdTrees)
                    {
                        var adTree = new AdTree(Morpheme.Epsilon(attributesModel), start);
                        adTree.Left = leftSubAdTree;

                        var morphemesCount = GetMorphemesCount(adTree);
                        if (morphemesCount <= maxMorphemes)
                        {
                            yield return adTree;
                        }
                    }
                }
            }
        }

        private static int GetMorphemesCount(IAdTree adTree)
        {
            var result = adTree.Root.Count(x => x.Pattern.IsLikeMorpheme || x.Left == null || x.Right == null);
            return result;
        }


        private static bool IsAdTreeCompleted(IAdTree adTree)
        {
            var root = adTree.Root;
            foreach (var adTreeItem in root)
            {
                if ((adTreeItem.Left == null || adTreeItem.Right == null) && !adTreeItem.Pattern.IsLikeMorpheme)
                {
                    return false;
                }
            }

            return true;
        }
    }
}
