using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Vertical.Utils.Rules;
using Krino.Vertical.Utils.Transformations;
using System.Diagnostics;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Constructions
{
    [DebuggerDisplay("{LeftRule} <- {Name} -> {RightRule}")]
    public class Pattern
    {
        public static Pattern O => new Pattern("O") { MorphemeRule = MorphemeRule.O_Lexeme, LeftRule = PatternRule.Nothing, RightRule = PatternRule.Nothing };
        public static Pattern I => new Pattern("I") { MorphemeRule = MorphemeRule.I_Lexeme, LeftRule = PatternRule.Nothing, RightRule = PatternRule.Nothing };
        public static Pattern A => new Pattern("A") { MorphemeRule = MorphemeRule.A, LeftRule = PatternRule.Nothing, RightRule = PatternRule.Nothing };
        public static Pattern E_Preposition => new Pattern("E Preposition") { MorphemeRule = MorphemeRule.E_Preposition, LeftRule = PatternRule.Nothing, RightRule = PatternRule.Nothing };
        public static Pattern E_adverb => new Pattern("E Adverb") { MorphemeRule = MorphemeRule.E_Adverb, LeftRule = PatternRule.Nothing, RightRule = PatternRule.Nothing };

        // Optional information for the debugging purposes.
        // It is used in the DebuggerDisplayAttribute.
        public string Name { get; private set; }

        public Pattern(string name = null)
        {
            Name = name;
        }

        public BigInteger PatternAttributes { get; set; }

        public MorphemeRule MorphemeRule { get; set; } = MorphemeRule.Nothing;


        public PatternRule LeftRule { get; set; } = PatternRule.Nothing;

        public PatternRule RightRule { get; set; } = PatternRule.Nothing;

        public ITransformation<Morpheme> Transference { get; set; } = Transferences.Transference.NothingToDo;

        public int ValencyPosition => PatternAttributesArrangement.PatternAttributes.ValencyPosition.GetValencyPosition(PatternAttributes);
    }
}
