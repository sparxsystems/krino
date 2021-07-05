using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Vertical.Utils.Collections;
using Krino.Vertical.Utils.Diagnostic;
using Krino.Vertical.Utils.Graphs;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions
{
    internal static class DirectedGraphExt
    {
        public static IEnumerable<AdTreeFactory> GetAdTreeFactories(this IDirectedGraph<Pattern, AdTreePosition> patternGraph, Pattern start, int maxMorphemes)
        {
            using var _t = Trace.Entering();

            var buffer = new DoubleKeyDictionary<int, Pattern, IEnumerable<AdTreeFactory>>();
            var result = GetPossibleAdTreesParallel(patternGraph, start, maxMorphemes, buffer);
            return result;
        }

        
        private static IEnumerable<AdTreeFactory> GetPossibleAdTreesParallel(IDirectedGraph<Pattern, AdTreePosition> patternGraph,
            Pattern start, int maxMorphemes,
            DoubleKeyDictionary<int, Pattern, IEnumerable<AdTreeFactory>> buffer)
        {
            using var _t = Trace.Entering();

            var result = new List<AdTreeFactory>();

            if (start.IsLikeMorpheme)
            {
                // Indicate the morpheme - end of the tree.
                result.Add(null);
            }
            // If non-morpheme pattern is still possible - it connects two morphemes, left and right.
            else if (maxMorphemes >= 2)
            {
                var parallelOptions = new ParallelOptions();
                if (maxMorphemes < 5)
                {
                    parallelOptions.MaxDegreeOfParallelism = 1;
                }

                // Get connecting non-morpheme patterns + 1 morpheme pattern.
                // Note: only one morpheme pattern is needed, otherwise the final result would be many same adtrees.
                var patternsOnRight = patternGraph.GetEdgesGoingFrom(start)
                    .Where(x => x.Value == AdTreePosition.ParrentForChildOnRight)
                    .Select(x => x.To);
                var nonMorphemePatternsOnRight = patternsOnRight.Where(x => !x.IsLikeMorpheme);
                var morphemePatternOnRight = patternsOnRight.FirstOrDefault(x => x.IsLikeMorpheme);
                var relevantPatternsOnRight = nonMorphemePatternsOnRight;
                if (morphemePatternOnRight != null)
                {
                    relevantPatternsOnRight = new Pattern[] { morphemePatternOnRight }.Concat(nonMorphemePatternsOnRight);
                }


                var patternsOnLeft = patternGraph.GetEdgesGoingFrom(start)
                    .Where(x => x.Value == AdTreePosition.ParrentForChildOnLeft)
                    .Select(x => x.To);
                var nonMorphemePatternsOnLeft = patternsOnLeft.Where(x => !x.IsLikeMorpheme);
                var morphemePatternOnLeft = patternsOnLeft.FirstOrDefault(x => x.IsLikeMorpheme);
                var relevantPatternsOnLeft = nonMorphemePatternsOnLeft;
                if (morphemePatternOnLeft != null)
                {
                    relevantPatternsOnLeft = new Pattern[] { morphemePatternOnLeft }.Concat(nonMorphemePatternsOnLeft);
                }

                if (relevantPatternsOnRight.Any() && relevantPatternsOnLeft.Any())
                {
                    Parallel.ForEach(relevantPatternsOnRight, parallelOptions, rightPattern =>
                    {
                        var rightSubAdTrees = GetAdTreesFromBuffer(buffer, patternGraph, rightPattern, maxMorphemes - 1);

                        foreach (var rightSubAdTree in rightSubAdTrees)
                        {
                            // Note: if rightPattern is a morpheme then the rightSubAdTree is null.

                            var morphemesOnRight = rightSubAdTree != null ? GetMorphemesCount(rightSubAdTree) : 1;

                            if (morphemesOnRight < maxMorphemes)
                            {
                                Parallel.ForEach(relevantPatternsOnLeft, parallelOptions, leftPattern =>
                                {
                                    var newMaxMorphemesOnLeft = maxMorphemes - morphemesOnRight;
                                    var leftSubAdTrees = GetAdTreesFromBuffer(buffer, patternGraph, leftPattern, newMaxMorphemesOnLeft);
                                    
                                    foreach (var leftSubAdTree in leftSubAdTrees)
                                    {
                                        var adTree = new AdTreeFactory(start);
                                        adTree.Right = rightSubAdTree;
                                        adTree.Left = leftSubAdTree;

                                        var morphemesCount = GetMorphemesCount(adTree);
                                        if (morphemesCount <= maxMorphemes)
                                        {
                                            lock (result)
                                            {
                                                result.Add(adTree);
                                            }
                                        }
                                    }
                                });
                            }
                        }
                    });

                    return result;
                }
                else if (relevantPatternsOnRight.Any())
                {
                    Parallel.ForEach(relevantPatternsOnRight, parallelOptions, rightPattern =>
                    {
                        var rightSubAdTrees = GetAdTreesFromBuffer(buffer, patternGraph, rightPattern, maxMorphemes - 1);
                        foreach (var rightSubAdTree in rightSubAdTrees)
                        {
                            var adTree = new AdTreeFactory(start);
                            adTree.Right = rightSubAdTree;

                            var morphemesCount = GetMorphemesCount(adTree);
                            if (morphemesCount <= maxMorphemes)
                            {
                                lock (result)
                                {
                                    result.Add(adTree);
                                }
                            }
                        }
                    });
                }
                else if (relevantPatternsOnLeft.Any())
                {
                    Parallel.ForEach(relevantPatternsOnLeft, parallelOptions, leftPattern =>
                    {
                        var leftSubAdTrees = GetAdTreesFromBuffer(buffer, patternGraph, leftPattern, maxMorphemes - 1);
                        foreach (var leftSubAdTree in leftSubAdTrees)
                        {
                            var adTree = new AdTreeFactory(start);
                            adTree.Left = leftSubAdTree;

                            var morphemesCount = GetMorphemesCount(adTree);
                            if (morphemesCount <= maxMorphemes)
                            {
                                lock (result)
                                {
                                    result.Add(adTree);
                                }
                            }
                        }
                    });
                }
            }

            return result;
        }

        private static IEnumerable<AdTreeFactory> GetAdTreesFromBuffer(DoubleKeyDictionary<int, Pattern, IEnumerable<AdTreeFactory>> buffer,
            IDirectedGraph<Pattern, AdTreePosition> patternGraph, Pattern pattern, int maxMorphemes)
        {
            using var _t = Trace.Entering();

            IEnumerable<AdTreeFactory> result = null;
            lock (buffer)
            {
                if (buffer.TryGetValue(maxMorphemes, pattern, out var adTrees))
                {
                    result = adTrees.ToList();
                }
            }

            if (result == null)
            {
                var adTrees = GetPossibleAdTreesParallel(patternGraph, pattern, maxMorphemes, buffer);

                lock (buffer)
                {
                    buffer[maxMorphemes, pattern] = adTrees;
                    result = adTrees.ToList();
                }
            }

            return result;
        }

        private static int GetMorphemesCount(AdTreeFactory adTreeFactory)
        {
            var result = adTreeFactory.Sum(x =>
            {
                int count = 0;
                if (x.Left == null)
                {
                    ++count;
                }
                if (x.Right == null)
                {
                    ++count;
                }
                if (x.Pattern.IsMorphematicAdPosition())
                {
                    ++count;
                }
                return count;
            });
            return result;
        }

    }
}
