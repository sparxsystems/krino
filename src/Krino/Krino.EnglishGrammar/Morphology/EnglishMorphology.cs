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

        public BigInteger GetAttributes(IEnumerable<IMorpheme> morphemes)
        {
            // Note: get prefixes from the root to the out. So the order must be reversed.
            var prefixes = morphemes.TakeWhile(x => GrammarAttributes.Morpheme.Bound.Prefix.IsIn(x.Attributes))
                .Where(x => x.Binding != null)
                .Reverse();

            var root = morphemes.FirstOrDefault(x => GrammarAttributes.Morpheme.Free.IsIn(x.Attributes));

            var suffixes = morphemes.SkipWhile(x => !GrammarAttributes.Morpheme.Bound.Suffix.IsIn(x.Attributes))
                .TakeWhile(x => GrammarAttributes.Morpheme.Bound.Suffix.IsIn(x.Attributes))
                .Where(x => x.Binding != null);

            // Attributes of the root morpheme.
            var result = root.Attributes;

            // Add prefixes and calculate attributes.
            foreach (var prefix in prefixes)
            {
                result = prefix.Binding.TransformAttributes(result);
            }

            // Add suffixes and calculate attributes.
            foreach (var suffix in suffixes)
            {
                result = suffix.Binding.TransformAttributes(result);
            }

            return result;
        }

        public string GetValue(IEnumerable<IMorpheme> morphemes)
        {
            // Note: get prefixes from the root to the out. So the order must be reversed.
            var prefixes = morphemes.TakeWhile(x => GrammarAttributes.Morpheme.Bound.Prefix.IsIn(x.Attributes))
                .Where(x => x.Binding != null)
                .Reverse();

            var root = morphemes.FirstOrDefault(x => GrammarAttributes.Morpheme.Free.IsIn(x.Attributes));

            var suffixes = morphemes.SkipWhile(x => !GrammarAttributes.Morpheme.Bound.Suffix.IsIn(x.Attributes))
                .TakeWhile(x => GrammarAttributes.Morpheme.Bound.Suffix.IsIn(x.Attributes))
                .Where(x => x.Binding != null);

            // Attributes of the root morpheme.
            var result = root.Value;

            // Add prefixes.
            foreach (var prefix in prefixes)
            {
                result = prefix.Binding.TransformValue(result);
            }

            // Add suffixes.
            foreach (var suffix in suffixes)
            {
                result = suffix.Binding.TransformValue(result);
            }

            return result;
        }
    }
}
