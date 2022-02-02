using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes;
using System.Collections.Generic;
using System.Linq;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    public class Word : LinguisticStructureBase, IWord
    {
        public Word(IEnumerable<IMorpheme> morphemes)
            : base(0)
        {
            var prefixes = morphemes.TakeWhile(x => GrammarAttributes.Morpheme.IsPrefix(x.Attributes));
            Prefixes.AddRange(prefixes);

            Root = morphemes.FirstOrDefault(x => GrammarAttributes.Morpheme.IsFreeMorpheme(x.Attributes));

            var suffixes = morphemes.TakeWhile(x => GrammarAttributes.Morpheme.IsSuffix(x.Attributes));
            Suffixes.AddRange(suffixes);
        }

        public List<IMorpheme> Prefixes { get; } = new List<IMorpheme>();

        public IMorpheme Root { get; set; }

        public List<IMorpheme> Suffixes { get; } = new List<IMorpheme>();

        public string Value
        {
            get
            {
                var morphemes = Prefixes.Cast<IMorpheme>().Append(Root).Concat(Suffixes).Where(x => x != null);
                var result = string.Join("", morphemes.Select(x => x.Value));
                return result;
            }
        }
    }
}
