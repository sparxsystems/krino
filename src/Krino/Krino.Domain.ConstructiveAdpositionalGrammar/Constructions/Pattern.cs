using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Vertical.Utils.Transformations;
using System.Diagnostics;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Constructions
{
    [DebuggerDisplay("{LeftRule} <- -> {RightRule}", Name = "{myName}")]
    public class Pattern : IPattern
    {
        public static Pattern O => new Pattern("O") { MorphemeRule = MorphemeRule.O, LeftRule = PatternRule.Nothing, RightRule = PatternRule.Nothing };
        public static Pattern I => new Pattern("I") { MorphemeRule = MorphemeRule.I, LeftRule = PatternRule.Nothing, RightRule = PatternRule.Nothing };
        public static Pattern A => new Pattern("A") { MorphemeRule = MorphemeRule.A, LeftRule = PatternRule.Nothing, RightRule = PatternRule.Nothing };
        public static Pattern E_Preposition => new Pattern("E Preposition") { MorphemeRule = MorphemeRule.E_Preposition, LeftRule = PatternRule.Nothing, RightRule = PatternRule.Nothing };
        public static Pattern E_adverb => new Pattern("E Adverb") { MorphemeRule = MorphemeRule.E_Adverb, LeftRule = PatternRule.Nothing, RightRule = PatternRule.Nothing };

        // Optional information for the debugging purposes.
        // It is used in the DebuggerDisplayAttribute.
        private string myName;

        public Pattern(string name = null)
        {
            myName = name;
        }

        public BigInteger PatternAttributes { get; set; }

        public MorphemeRule MorphemeRule { get; set; } = MorphemeRule.Anything;


        public PatternRule LeftRule { get; set; } = PatternRule.Anything;

        public PatternRule RightRule { get; set; } = PatternRule.Anything;

        public ITransformation<IMorpheme> Transference { get; set; } = Transferences.Transference.NothingToDo;

        public int ValencyPosition => PatternAttributesArrangement.PatternAttributes.ValencyPosition.GetValencyPosition(PatternAttributes);
    }
}
