using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    [DebuggerDisplay("{DebuggerDisplay}")]
    public class Word : LinguisticStructureBase, IWord
    {
        public Word(string word, BigInteger attributes) : this(new IMorpheme[] { new Morpheme(word, attributes) })
        {
            Attributes = attributes;
        }

        public Word(IMorpheme rootMorpheme) : this(new IMorpheme[] { rootMorpheme })
        {
            Attributes = rootMorpheme.Attributes;
        }

        public Word(IEnumerable<IMorpheme> morphemes)
            : base(0)
        {
            var prefixes = morphemes.TakeWhile(x => GrammarAttributes.Morpheme.IsPrefix(x.Attributes));
            Prefixes.AddRange(prefixes);

            Root = morphemes.FirstOrDefault(x => GrammarAttributes.Morpheme.IsFreeMorpheme(x.Attributes));

            var suffixes = morphemes.SkipWhile(x => !GrammarAttributes.Morpheme.IsSuffix(x.Attributes))
                .TakeWhile(x => GrammarAttributes.Morpheme.IsSuffix(x.Attributes));

            Suffixes.AddRange(suffixes);
        }

        public List<IMorpheme> Prefixes { get; } = new List<IMorpheme>();

        public IMorpheme Root { get; set; }

        public List<IMorpheme> Suffixes { get; } = new List<IMorpheme>();

        public IEnumerable<IMorpheme> Morphemes => Prefixes.Append(Root).Concat(Suffixes).Where(x => x != null);

        public string Value => string.Join("", Morphemes.Select(x => x.Value));

        private string DebuggerDisplay => string.Join(" : ", Value, AttributesStr);
    }
}
