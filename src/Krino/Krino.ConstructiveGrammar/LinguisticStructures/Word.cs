using Krino.ConstructiveGrammar.LinguisticStructures.Attributes;
using Krino.ConstructiveGrammar.Morphology;
using Krino.Vertical.Utils.Collections;
using Krino.Vertical.Utils.Enums;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Numerics;
using System.Text;

namespace Krino.ConstructiveGrammar.LinguisticStructures
{
    [DebuggerDisplay("{DebuggerDisplay}")]
    public class Word : LinguisticStructureBase, IWord
    {
        private IMorphology myMorphology;

        public Word(IMorphology morphology, string word, BigInteger attributes) : this(morphology, new IMorpheme[] { new Morpheme(word, attributes) })
        {
        }

        public Word(IMorphology morphology, IMorpheme rootMorpheme) : this(morphology, new IMorpheme[] { rootMorpheme })
        {
        }

        public Word(IMorphology morphology, IEnumerable<IMorpheme> morphemes)
            : base(0)
        {
            myMorphology = morphology;

            if (morphemes.IsSingle())
            {
                Root = morphemes.FirstOrDefault();
            }
            else
            {
                var prefixes = morphemes.TakeWhile(x => GrammarAttributes.Morpheme.Bound.Prefix.IsIn(x.Attributes));
                Prefixes.AddRange(prefixes);

                Root = morphemes.FirstOrDefault(x => GrammarAttributes.Morpheme.Free.IsIn(x.Attributes));

                var suffixes = morphemes.SkipWhile(x => !GrammarAttributes.Morpheme.Bound.Suffix.IsIn(x.Attributes))
                    .TakeWhile(x => GrammarAttributes.Morpheme.Bound.Suffix.IsIn(x.Attributes));

                Suffixes.AddRange(suffixes);
            }
        }

        public override BigInteger Attributes { get => GetAttributes(); protected set { } }

        public List<IMorpheme> Prefixes { get; } = new List<IMorpheme>();

        public IMorpheme Root { get; set; }

        public List<IMorpheme> Suffixes { get; } = new List<IMorpheme>();

        public IEnumerable<IMorpheme> Morphemes => Prefixes.Append(Root).Concat(Suffixes).Where(x => x != null);

        public string Value => myMorphology.GetValue(Morphemes);

        public string GrammarStr => string.Join("", AttributesStr, "(", Value, ")");


        public void BindMorpheme(IMorpheme morpheme)
        {
            if (GrammarAttributes.Morpheme.Bound.Prefix.IsIn(morpheme.Attributes))
            {
                Prefixes.Add(morpheme);
            }
            else if (GrammarAttributes.Morpheme.Bound.Suffix.IsIn(morpheme.Attributes))
            {
                Suffixes.Add(morpheme);
            }
        }


        public void BuildFormattedGrammarStr(int indent, StringBuilder builder)
        {
            builder.Append(new string(' ', indent)).Append(Value).Append(" : ").AppendLine(AttributesStr);

            //Prefixes.ForEach(x => x.BuildFormattedGrammarStr(indent + 4, builder));
            //Root.BuildFormattedGrammarStr(indent + 4, builder);
            //Suffixes.ForEach(x => x.BuildFormattedGrammarStr(indent + 4, builder));
        }

        public ILinguisticStructure DeepCopy()
        {
            var morphemes = Morphemes.Select(x => x.DeepCopy()).OfType<IMorpheme>();
            var result = new Word(myMorphology, morphemes);
            return result;
        }


        private BigInteger GetAttributes() => myMorphology.GetAttributes(Morphemes);


        private string DebuggerDisplay => string.Join(" : ", Value, AttributesStr);
    }
}
