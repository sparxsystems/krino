using Krino.ConstructiveGrammar.Dictionary;
using Krino.ConstructiveGrammar.LinguisticStructures;
using Krino.ConstructiveGrammar.LinguisticStructures.Attributes;
using Krino.ConstructiveGrammar.Morphology;
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

        public BigInteger GetAttributes(IEnumerable<IMorpheme> morphemes)
        {
            var decomp = Decompose(morphemes);

            // Attributes of root morphemes. E.g. wheelchair - the last root 'chair' defines the root attributes.
            var result = decomp.Roots.Last().Attributes;

            // Add prefixes and calculate attributes.
            foreach (var prefix in decomp.Prefixes)
            {
                result = prefix.Binding.TransformAttributes(result);
            }

            // Add suffixes and calculate attributes.
            foreach (var suffix in decomp.Suffixes)
            {
                result = suffix.Binding.TransformAttributes(result);
            }

            return result;
        }

        public string GetValue(IEnumerable<IMorpheme> morphemes)
        {
            var decomp = Decompose(morphemes);

            // Attributes of the root morpheme.
            var result = string.Concat(decomp.Roots.Select(x => x.Value));

            // Add prefixes.
            foreach (var prefix in decomp.Prefixes)
            {
                result = prefix.Binding.TransformValue(result);
            }

            // Add suffixes.
            foreach (var suffix in decomp.Suffixes)
            {
                result = suffix.Binding.TransformValue(result);
            }

            return result;
        }
    }
}
