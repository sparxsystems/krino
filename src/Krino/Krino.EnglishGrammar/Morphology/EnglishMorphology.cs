using Krino.ConstructiveGrammar.Dictionary;
using Krino.ConstructiveGrammar.LinguisticStructures;
using Krino.ConstructiveGrammar.LinguisticStructures.Attributes;
using Krino.ConstructiveGrammar.Morphology;
using Krino.Vertical.Utils.Collections;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;

namespace Krino.EnglishGrammar.Morphology
{
    public class EnglishMorphology : IMorphology
    {
        public EnglishMorphology()
        {
        }

        public (IEnumerable<IMorpheme> Prefixes, IEnumerable<IMorpheme> Roots, IEnumerable<IMorpheme> Suffixes) Decompose(IEnumerable<IMorpheme> morphemes)
        {
            using var _ = Krino.Vertical.Utils.Diagnostic.Trace.Entering();

            // Note: get prefixes from the root to the out. So the order must be reversed.
            var prefixes = morphemes.TakeWhile(x => GrammarAttributes.Morpheme.Bound.Prefix.IsIn(x.Attributes))
                .Where(x => x.Binding != null)
                .Reverse();

            var root = morphemes.SkipWhile(x => !GrammarAttributes.Morpheme.Bound.Root.IsIn(x.Attributes) && !GrammarAttributes.Morpheme.Free.IsIn(x.Attributes))
                .TakeWhile(x => GrammarAttributes.Morpheme.Bound.Root.IsIn(x.Attributes) || GrammarAttributes.Morpheme.Free.IsIn(x.Attributes));

            var suffixes = morphemes.SkipWhile(x => !GrammarAttributes.Morpheme.Bound.Suffix.IsIn(x.Attributes))
                .TakeWhile(x => GrammarAttributes.Morpheme.Bound.Suffix.IsIn(x.Attributes))
                .Where(x => x.Binding != null);

            return (prefixes, root, suffixes);
        }

        public IEnumerable<IEnumerable<IMorpheme>> GetDerivationSequences(IEnumerable<IMorpheme> morphemes)
        {
            using var _ = Krino.Vertical.Utils.Diagnostic.Trace.Entering();

            IEnumerable<List<IMorpheme>> result;


            if (morphemes.IsSingle())
            {
                result = new List<List<IMorpheme>>() { morphemes.ToList() };
            }
            else
            {
                var decomposition = Decompose(morphemes);
                if (decomposition.Roots.Any())
                {
                    var prefixes = decomposition.Prefixes.ToList();
                    var roots = decomposition.Roots.Reverse().ToList();
                    var suffixes = decomposition.Suffixes.ToList();
                    var tmpResult = new List<IMorpheme>();

                    var allCombinations = GetAllMorphemeCombinations(0, 0, 0, prefixes, roots, suffixes, tmpResult);
                    result = allCombinations.Where(x => IsMeaningful(x));
                }
                else
                {
                    result = Enumerable.Empty<List<IMorpheme>>();
                }
            }

            return result;
        }

        public BigInteger GetAttributes(IEnumerable<IMorpheme> morphemes)
        {
            using var _ = Krino.Vertical.Utils.Diagnostic.Trace.Entering();

            BigInteger result = 0;

            var derivationSequence = GetDerivationSequences(morphemes).FirstOrDefault();
            if (derivationSequence != null && derivationSequence.Any())
            {
                result = derivationSequence.First().Attributes;

                foreach (var morpheme in derivationSequence.Skip(1))
                {
                    // Note: prefixes and suffixes has defined binding.
                    if (morpheme.Binding != null)
                    {
                        result = morpheme.Binding.TransformAttributes(result);
                    }
                    // Note: compound roots has not defined binding by default.
                    else
                    {
                        // Note: other root is put before the main root and it does not affect attributes.
                    }
                }
            }

            return result;
        }

        public string GetValue(IEnumerable<IMorpheme> morphemes)
        {
            using var _ = Krino.Vertical.Utils.Diagnostic.Trace.Entering();

            string result = "";

            var derivationSequence = GetDerivationSequences(morphemes).FirstOrDefault();
            if (derivationSequence != null)
            {
                foreach (var morpheme in derivationSequence)
                {
                    // Note: prefixes and suffixes has defined binding.
                    if (morpheme.Binding != null)
                    {
                        result = morpheme.Binding.TransformValue(result);
                    }
                    // Note: compound roots has not defined binding by default.
                    else
                    {
                        // Note: other root is put before the main root.
                        result = string.Concat(morpheme.Value, result);
                    }
                }
            }

            return result;
        }

        private IEnumerable<List<IMorpheme>> GetAllMorphemeCombinations(
            int prefixIdx, int rootIdx, int suffixIdx,
            List<IMorpheme> prefixes, List<IMorpheme> roots, List<IMorpheme> suffixes,
            List<IMorpheme> tmpResult)
        {
            using var _ = Krino.Vertical.Utils.Diagnostic.Trace.Entering();

            if (prefixIdx >= prefixes.Count && rootIdx >= roots.Count && suffixIdx >= suffixes.Count)
            {
                // All morphemes are used so retun the result.
                yield return tmpResult.ToList();
            }
            else
            {
                if (rootIdx < roots.Count)
                {
                    var morpheme = roots[rootIdx];
                    tmpResult.Add(morpheme);

                    var results = GetAllMorphemeCombinations(prefixIdx, rootIdx + 1, suffixIdx, prefixes, roots, suffixes, tmpResult);
                    foreach (var result in results)
                    {
                        yield return result;
                    }

                    tmpResult.RemoveAt(tmpResult.Count - 1);
                }
                else
                {
                    if (prefixIdx < prefixes.Count)
                    {
                        var morpheme = prefixes[prefixIdx];
                        tmpResult.Add(morpheme);

                        var results = GetAllMorphemeCombinations(prefixIdx + 1, rootIdx, suffixIdx, prefixes, roots, suffixes, tmpResult);
                        foreach (var result in results)
                        {
                            yield return result;
                        }

                        tmpResult.RemoveAt(tmpResult.Count - 1);
                    }

                    if (suffixIdx < suffixes.Count)
                    {
                        var morpheme = suffixes[suffixIdx];
                        tmpResult.Add(morpheme);

                        var results = GetAllMorphemeCombinations(prefixIdx, rootIdx, suffixIdx + 1, prefixes, roots, suffixes, tmpResult);
                        foreach (var result in results)
                        {
                            yield return result;
                        }

                        tmpResult.RemoveAt(tmpResult.Count - 1);
                    }
                }
            }
            
        }

        private bool IsMeaningful(IEnumerable<IMorpheme> morphemes)
        {
            using var _ = Krino.Vertical.Utils.Diagnostic.Trace.Entering();

            IWord word = null;

            foreach (var morpheme in morphemes)
            {
                if (word == null)
                {
                    word = new Word(this, morpheme);
                }
                else
                {
                    if (word.CanBind(morpheme))
                    {
                        word.Bind(morpheme);
                    }
                    else
                    {
                        return false;
                    }
                }
            }

            return true;
        }

        

    }
}
