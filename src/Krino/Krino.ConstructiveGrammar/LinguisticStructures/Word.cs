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
                Roots.Add(morphemes.FirstOrDefault());
            }
            else
            {
                var decomp = myMorphology.Decompose(morphemes);

                Prefixes.AddRange(decomp.Prefixes.Reverse());
                Roots.AddRange(decomp.Roots);
                Suffixes.AddRange(decomp.Suffixes);
            }
        }

        public override BigInteger Attributes { get => GetAttributes(); protected set { } }

        public List<IMorpheme> Prefixes { get; } = new List<IMorpheme>();

        public List<IMorpheme> Roots { get; set; } = new List<IMorpheme>();

        public List<IMorpheme> Suffixes { get; } = new List<IMorpheme>();

        public IEnumerable<IMorpheme> Morphemes => Prefixes.Concat(Roots).Concat(Suffixes).Where(x => x != null);

        public string Value => myMorphology.GetValue(Morphemes);

        public string GrammarStr => string.Join("", AttributesStr, "(", Value, ")");


        public bool CanBind(IMorpheme morpheme)
        {
            bool result = false;

            // If prefix or suffix.
            if (GrammarAttributes.Morpheme.Bound.Prefix.IsIn(morpheme.Attributes) ||
                GrammarAttributes.Morpheme.Bound.Suffix.IsIn(morpheme.Attributes))
            {
                result = morpheme.Binding.CanBind(this);
            }
            // If compound roots.
            else if (!Prefixes.Any() && GrammarAttributes.Morpheme.Free.Lexical.IsIn(morpheme.Attributes))
            {
                result = true;
            }

            return result;
        }

        public void Bind(IMorpheme morpheme)
        {
            if (GrammarAttributes.Morpheme.Bound.Prefix.IsIn(morpheme.Attributes))
            {
                // Note: the order of prefixes is from inside to outside.
                Prefixes.Add(morpheme);
            }
            else if (GrammarAttributes.Morpheme.Bound.Suffix.IsIn(morpheme.Attributes))
            {
                Suffixes.Add(morpheme);
            }
            else
            {
                // Compound roots.
                Roots.Insert(0, morpheme);
            }
        }


        public void BuildFormattedGrammarStr(int indent, StringBuilder builder)
        {
            builder.Append(new string(' ', indent)).Append(Value).Append(" : ").AppendLine(AttributesStr);

            Prefixes.ForEach(x => x.BuildFormattedGrammarStr(indent + 4, builder));
            Roots.ForEach(x => x.BuildFormattedGrammarStr(indent + 4, builder));
            Suffixes.ForEach(x => x.BuildFormattedGrammarStr(indent + 4, builder));
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
