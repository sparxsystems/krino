using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions.Rules;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Vertical.Utils.Collections;
using Krino.Vertical.Utils.Diagnostic;
using Krino.Vertical.Utils.Graphs;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions
{
    internal static class PatternConstructionsExt
    {
        private static readonly List<AdTreeFactory> myNullItemList = new List<AdTreeFactory>() { null };

        public static int Writings { get; private set; }

        public static DirectedGraph<Pattern, AdTreePosition> CreatePatternGraph(this IEnumerable<Pattern> patterns)
        {
            using var _t = Trace.Entering();

            var result = new DirectedGraph<Pattern, AdTreePosition>();
            result.AddVertices(patterns);

            foreach (var fromPattern in patterns)
            {
                foreach (var toPattern in patterns)
                {
                    if (fromPattern.RightPatternRule.Evaluate(toPattern))
                    {
                        result.AddEdge(fromPattern, toPattern, AdTreePosition.ParrentForChildOnRight);
                    }

                    if (fromPattern.LeftPatternRule.Evaluate(toPattern))
                    {
                        result.AddEdge(fromPattern, toPattern, AdTreePosition.ParrentForChildOnLeft);
                    }
                }
            }

            return result;
        }


        public static IEnumerable<AdTreeFactory> GetAdTreeFactories(this IDirectedGraph<Pattern, AdTreePosition> patternGraph,
            Pattern start, int maxMorphemes)
        {
            using var _t = Trace.Entering();

            Writings = 0;

            var buffer = new DoubleKeyDictionary<int, Pattern, IEnumerable<AdTreeFactory>>();
            var result = GetPossibleFactories(patternGraph, start, maxMorphemes, buffer);
            return result;
        }

        public static IReadOnlyList<AdTreeFactory> GetAllAdTreeFactories(this IDirectedGraph<Pattern, AdTreePosition> patternGraph, int maxMorphemes)
        {
            using var _t = Trace.Entering();

            Writings = 0;

            var factories = new List<AdTreeFactory>();

            var rootPatterns = patternGraph.Where(x => !x.IsLikeMorpheme).ToList();

            var buffer = new DoubleKeyDictionary<int, Pattern, IEnumerable<AdTreeFactory>>();

            foreach (var pattern in rootPatterns)
            //Parallel.ForEach(rootPatterns, pattern =>
            {
                var possibleAdTreePatterns = GetPossibleFactories(patternGraph, pattern, maxMorphemes, buffer);
                factories.AddRange(possibleAdTreePatterns);
            }//);

            var adTreeFactoryComparer = new AdTreeFactoryComparer();
            var result = factories.Distinct(adTreeFactoryComparer).ToList();

            return result;
        }

        public static IReadOnlyList<AdTreeFactory> GetAdTreeFactoriesForPatternSignature(this IDirectedGraph<Pattern, AdTreePosition> patternGraph,
            string patternSignature, PatternConstructionsBuffer buffer)
        {
            using var _t = Trace.Entering();

            Writings = 0;

            var factories = new List<AdTreeFactory>();

            var rootPatterns = patternSignature[patternSignature.Length - 1] == 'U' ?
                // Sentence ending with a punctuation mark.
                patternGraph.Where(x => x.IsMorphematicAdPosition && (x.LeftRule == MorphemeRule.Nothing || x.RightRule == MorphemeRule.Nothing)).ToList() :
                // Phrase NOT ending with a punctuation mark.
                patternGraph.Where(x => !x.IsLikeMorpheme || x.IsMorphematicAdPosition && x.LeftRule != MorphemeRule.Nothing && x.RightRule != MorphemeRule.Nothing).ToList();
            
            foreach (var pattern in rootPatterns)
            {
                var possibleAdTreePatterns = GetFactoriesForPatternSignature(patternGraph, pattern, patternSignature, patternSignature.Length, buffer);

                if (possibleAdTreePatterns != null && possibleAdTreePatterns.Any())
                {
                    factories.AddRange(possibleAdTreePatterns);
                }
            }

            var adTreeFactoryComparer = new AdTreeFactoryComparer();
            var result = factories.Distinct(adTreeFactoryComparer).ToList();

            return result;
        }


        private static IEnumerable<AdTreeFactory> GetFactoriesForPatternSignature(
            IDirectedGraph<Pattern, AdTreePosition> patternGraph,
            Pattern start, string patternSignature, int requiredMorphemesCount,
            PatternConstructionsBuffer buffer)
        {
            using var _t = Trace.Entering();


            List<AdTreeFactory> result;

            if (start.IsLikeMorpheme)
            {
                result = myNullItemList;
            }
            // If there are available morpheme slots to perform searching.
            else if (requiredMorphemesCount >= 3 || !start.IsMorphematicAdPosition && requiredMorphemesCount >= 2)
            {
                // If the result shall be in the buffer.
                if (requiredMorphemesCount <= buffer.MaxMorphemes)
                {
                    var subPatternSignature = patternSignature.Substring(0, requiredMorphemesCount);
                    var bufferedFactories = buffer.GetAdTreeFactories(subPatternSignature, start);
                    result = bufferedFactories != null ? bufferedFactories.ToList() : new List<AdTreeFactory>(0);
                }
                else
                {
                    result = new List<AdTreeFactory>();

                    if (start.IsLeftFirst)
                    {
                        var leftPatterns = GetRelevantPatterns(patternGraph, start, AdTreePosition.ParrentForChildOnLeft);
                        foreach (var leftPattern in leftPatterns)
                        {
                            var leftAdTreeFactories = GetFactoriesForPatternSignature(patternGraph, leftPattern, patternSignature, requiredMorphemesCount - 1, buffer);
                            foreach (var leftAdTreeFactory in leftAdTreeFactories)
                            {
                                // If it is a morpheme then check the grammar character.
                                if (leftAdTreeFactory == null)
                                {
                                    var leftGrammarCharacter = Enum.Parse<GrammarCharacter>(patternSignature[0].ToString());
                                    if (start.LeftRule.GrammarCharacter != leftGrammarCharacter)
                                    {
                                        break;
                                    }
                                }

                                var leftMorphemesCount = leftAdTreeFactory != null ? GetMorphemesCount(leftAdTreeFactory) : 1;

                                // If morphematic adPosition then check the grammar character.
                                var morphematicAdPositionCount = 0;
                                if (start.IsMorphematicAdPosition)
                                {
                                    var adPositionGrammarCharacter = Enum.Parse<GrammarCharacter>(patternSignature[leftMorphemesCount].ToString());
                                    if (start.UpRule.GrammarCharacter != adPositionGrammarCharacter)
                                    {
                                        break;
                                    }

                                    morphematicAdPositionCount = 1;

                                    // If it is the end. E.g. end of a sentence.
                                    if (leftMorphemesCount + morphematicAdPositionCount >= patternSignature.Length &&
                                        start.LeftRule == MorphemeRule.Nothing)
                                    {
                                        var adTreeFactory = new AdTreeFactory(start);
                                        adTreeFactory.Right = null;
                                        adTreeFactory.Left = leftAdTreeFactory;

                                        var morphemesCount = GetMorphemesCount(adTreeFactory);
                                        if (morphemesCount == requiredMorphemesCount)
                                        {
                                            lock (result)
                                            {
                                                result.Add(adTreeFactory);
                                                break;
                                            }
                                        }
                                    }
                                }

                                if (leftMorphemesCount + morphematicAdPositionCount >= patternSignature.Length)
                                {
                                    break;
                                }

                                var newRightMaxMorphemes = requiredMorphemesCount - leftMorphemesCount - morphematicAdPositionCount;
                                var newLeftPatternSignature = patternSignature.Substring(leftMorphemesCount + morphematicAdPositionCount);

                                var rightPatterns = GetRelevantPatterns(patternGraph, start, AdTreePosition.ParrentForChildOnRight);
                                foreach (var rightPattern in rightPatterns)
                                {
                                    var rightAdTreeFactories = GetFactoriesForPatternSignature(patternGraph, rightPattern, newLeftPatternSignature, newRightMaxMorphemes, buffer);
                                    foreach (var rightAdTreeFactory in rightAdTreeFactories)
                                    {
                                        // If it is a morpheme then check the grammar character.
                                        if (rightAdTreeFactory == null)
                                        {
                                            var rightGrammarCharacter = Enum.Parse<GrammarCharacter>(patternSignature[leftMorphemesCount + morphematicAdPositionCount].ToString());
                                            if (start.RightRule.GrammarCharacter != rightGrammarCharacter)
                                            {
                                                break;
                                            }
                                        }

                                        var adTreeFactory = new AdTreeFactory(start);
                                        adTreeFactory.Right = rightAdTreeFactory;
                                        adTreeFactory.Left = leftAdTreeFactory;

                                        var morphemesCount = GetMorphemesCount(adTreeFactory);
                                        if (morphemesCount == requiredMorphemesCount)
                                        {
                                            lock (result)
                                            {
                                                result.Add(adTreeFactory);
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                    else
                    {
                        var rightPatterns = GetRelevantPatterns(patternGraph, start, AdTreePosition.ParrentForChildOnRight);
                        foreach (var rightPattern in rightPatterns)
                        {
                            var rightAdTreeFactories = GetFactoriesForPatternSignature(patternGraph, rightPattern, patternSignature, requiredMorphemesCount - 1, buffer);
                            foreach (var rightAdTreeFactory in rightAdTreeFactories)
                            {
                                // If it is a morpheme then check the grammar character.
                                if (rightAdTreeFactory == null)
                                {
                                    var rightGrammarCharacter = Enum.Parse<GrammarCharacter>(patternSignature[0].ToString());
                                    if (start.RightRule.GrammarCharacter != rightGrammarCharacter)
                                    {
                                        break;
                                    }
                                }

                                var rightMorphemesCount = rightAdTreeFactory != null ? GetMorphemesCount(rightAdTreeFactory) : 1;

                                // If morphematic adPosition then check the grammar character.
                                var morphematicAdPositionCount = 0;
                                if (start.IsMorphematicAdPosition)
                                {
                                    var adPositionGrammarCharacter = Enum.Parse<GrammarCharacter>(patternSignature[rightMorphemesCount].ToString());
                                    if (start.UpRule.GrammarCharacter != adPositionGrammarCharacter)
                                    {
                                        break;
                                    }

                                    morphematicAdPositionCount = 1;

                                    // If it is the end. E.g. end of a sentence.
                                    if (rightMorphemesCount + morphematicAdPositionCount >= patternSignature.Length &&
                                        start.LeftRule == MorphemeRule.Nothing)
                                    {
                                        var adTreeFactory = new AdTreeFactory(start);
                                        adTreeFactory.Right = rightAdTreeFactory;
                                        adTreeFactory.Left = null;

                                        var morphemesCount = GetMorphemesCount(adTreeFactory);
                                        if (morphemesCount == requiredMorphemesCount)
                                        {
                                            lock (result)
                                            {
                                                result.Add(adTreeFactory);
                                                break;
                                            }
                                        }
                                    }
                                }

                                if (rightMorphemesCount + morphematicAdPositionCount >= patternSignature.Length)
                                {
                                    break;
                                }

                                var newLeftMaxMorphemes = requiredMorphemesCount - rightMorphemesCount - morphematicAdPositionCount;
                                var newLeftPatternSignature = patternSignature.Substring(rightMorphemesCount + morphematicAdPositionCount);

                                var leftPatterns = GetRelevantPatterns(patternGraph, start, AdTreePosition.ParrentForChildOnLeft);
                                foreach (var leftPattern in leftPatterns)
                                {
                                    var leftAdTreeFactories = GetFactoriesForPatternSignature(patternGraph, leftPattern, newLeftPatternSignature, newLeftMaxMorphemes, buffer);
                                    foreach (var leftAdTreeFactory in leftAdTreeFactories)
                                    {
                                        // If it is a morpheme then check the grammar character.
                                        if (leftAdTreeFactory == null)
                                        {
                                            var leftGrammarCharacter = Enum.Parse<GrammarCharacter>(patternSignature[rightMorphemesCount + morphematicAdPositionCount].ToString());
                                            if (start.LeftRule.GrammarCharacter != leftGrammarCharacter)
                                            {
                                                break;
                                            }
                                        }

                                        var adTreeFactory = new AdTreeFactory(start);
                                        adTreeFactory.Right = rightAdTreeFactory;
                                        adTreeFactory.Left = leftAdTreeFactory;

                                        var morphemesCount = GetMorphemesCount(adTreeFactory);
                                        if (morphemesCount == requiredMorphemesCount)
                                        {
                                            lock (result)
                                            {
                                                result.Add(adTreeFactory);
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            else
            {
                result = new List<AdTreeFactory>(0);
            }

            return result;
        }

        private static IEnumerable<Pattern> GetRelevantPatterns(IDirectedGraph<Pattern, AdTreePosition> patternGraph,
            Pattern start, AdTreePosition parentPosition)
        {
            var patterns = patternGraph.GetEdgesGoingFrom(start)
                    .Where(x => x.Value == parentPosition)
                    .Select(x => x.To);
            var nonMorphemePatterns = patterns.Where(x => !x.IsLikeMorpheme);
            var morphemePattern = patterns.FirstOrDefault(x => x.IsLikeMorpheme);
            var relevantPatterns = morphemePattern != null ?
                new Pattern[] { morphemePattern }.Concat(nonMorphemePatterns) :
                nonMorphemePatterns;

            return relevantPatterns;
        }

        private static List<AdTreeFactory> TryGetFactoriesFromBuffer(Dictionary<string, HashSet<AdTreeFactory>> buffer,
            ReaderWriterLockSlim readerWriterLock,
            string patternSignature,
            Pattern pattern)
        {
            List<AdTreeFactory> result = null;

            readerWriterLock.EnterReadLock();
            try
            {
                if (buffer.TryGetValue(patternSignature, out var factories))
                {
                    result = factories.Where(x => x.Pattern == pattern).ToList();
                }
            }
            finally
            {
                readerWriterLock.ExitReadLock();
            }

            return result;
        }


        private static IEnumerable<AdTreeFactory> GetPossibleFactories(IDirectedGraph<Pattern, AdTreePosition> patternGraph,
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
            else if (maxMorphemes >= 3 || !start.IsMorphematicAdPosition && maxMorphemes >= 2)
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
                    //foreach (var rightPattern in relevantPatternsOnRight)
                    Parallel.ForEach(relevantPatternsOnRight, parallelOptions, rightPattern =>
                    {
                        var rightSubAdTrees = GetAdTreesFromBuffer(buffer, patternGraph, rightPattern, maxMorphemes - 1);

                        foreach (var rightSubAdTree in rightSubAdTrees)
                        {
                            // Note: if rightPattern is a morpheme then the rightSubAdTree is null.

                            var morphemesOnRight = rightSubAdTree != null ? GetMorphemesCount(rightSubAdTree) : 1;

                            if (morphemesOnRight < maxMorphemes)
                            {
                                foreach (var leftPattern in relevantPatternsOnLeft)
                                //Parallel.ForEach(relevantPatternsOnLeft, parallelOptions, leftPattern =>
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
                                }//);
                            }
                        }
                    });

                    return result;
                }
                else if (relevantPatternsOnRight.Any())
                {
                    //foreach (var rightPattern in relevantPatternsOnRight)
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
                    //foreach (var leftPattern in relevantPatternsOnLeft)
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

            if (pattern.IsLikeMorpheme)
            {
                return myNullItemList;
            }

            IEnumerable<AdTreeFactory> result = null;
            buffer.ReaderWriterLock.EnterReadLock();
            try
            {
                if (buffer.TryGetValue(maxMorphemes, pattern, out var adTrees))
                {
                    result = adTrees.ToList();
                }
            }
            finally
            {
                buffer.ReaderWriterLock.ExitReadLock();
            }

            if (result == null)
            {
                var adTrees = GetPossibleFactories(patternGraph, pattern, maxMorphemes, buffer);

                buffer.ReaderWriterLock.EnterWriteLock();
                try
                {
                    result = adTrees.ToHashSet(new AdTreeFactoryComparer());
                    buffer[maxMorphemes, pattern] = result;

                    ++Writings;
                }
                finally
                {
                    buffer.ReaderWriterLock.ExitWriteLock();
                }
            }

            return result;
        }

        private static int GetMorphemesCount(AdTreeFactory adTreeFactory)
        {
            using var _t = Trace.Entering();

            var result = adTreeFactory.Sum(x =>
            {
                int count = 0;
                if (x.Left == null && x.Pattern.LeftRule != MorphemeRule.Nothing)
                {
                    ++count;
                }
                if (x.Right == null && x.Pattern.RightRule != MorphemeRule.Nothing)
                {
                    ++count;
                }
                if (x.Pattern.IsMorphematicAdPosition)
                {
                    ++count;
                }
                return count;
            });
            return result;
        }

    }
}
