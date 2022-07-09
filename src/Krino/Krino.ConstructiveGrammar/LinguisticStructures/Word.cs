using Krino.ConstructiveGrammar.LinguisticStructures.Attributes;
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
        public Word(string word, BigInteger attributes) : this(new IMorpheme[] { new Morpheme(word, attributes) })
        {
        }

        public Word(IMorpheme rootMorpheme) : this(new IMorpheme[] { rootMorpheme })
        {
        }

        public Word(IEnumerable<IMorpheme> morphemes)
            : base(0)
        {
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

        public string Value => string.Join("", Morphemes.Select(x => x.Value));

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
            var result = new Word(morphemes);
            return result;
        }


        private BigInteger GetAttributes()
        {
            BigInteger result = Root.Attributes;

            // Note: iterate from the root to the out. So the order must be reversed.
            var prefixes = Prefixes.Where(x => x.Binding != null).Reverse();
            foreach (var prefix in prefixes)
            { 
                var enumsToRemove = GrammarAttributes.Instance.FindEnums(prefix.Binding.AttributesToRemove);
                foreach (var enumToRemove in enumsToRemove)
                {
                    if (enumsToRemove is EnumValue enumValueToRemove)
                    {
                        result = enumValueToRemove.Clear(result);
                    }
                    else if (enumsToRemove is EnumGroupBase enumGroupToRemove)
                    {
                        result = enumGroupToRemove.Clear(result);
                    }
                }

                result |= prefix.Binding.AttributesToAdd;
            }

            var suffixes = Suffixes.Where(x => x.Binding != null);
            foreach (var suffix in suffixes)
            {
                var enumsToRemove = GrammarAttributes.Instance.FindEnums(suffix.Binding.AttributesToRemove);
                foreach (var enumToRemove in enumsToRemove)
                {
                    if (enumsToRemove is EnumValue enumValueToRemove)
                    {
                        result = enumValueToRemove.Clear(result);
                    }
                    else if (enumsToRemove is EnumGroupBase enumGroupToRemove)
                    {
                        result = enumGroupToRemove.Clear(result);
                    }
                }

                result |= suffix.Binding.AttributesToAdd;
            }

            return result;
        }


        private string DebuggerDisplay => string.Join(" : ", Value, AttributesStr);
    }
}
