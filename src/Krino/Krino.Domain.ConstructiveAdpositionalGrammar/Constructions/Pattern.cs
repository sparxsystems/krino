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
        public Pattern(string name = null)
        {
            Name = name;
        }

        // Optional information for the debugging purposes.
        public string Name { get; private set; }

        public BigInteger PatternAttributes { get; set; }

        public MorphemeRule MorphemeRule { get; set; } = MorphemeRule.Nothing;


        public PatternRule LeftRule { get; set; } = PatternRule.Nothing;

        public PatternRule RightRule { get; set; } = PatternRule.Nothing;

        public ITransformation<Morpheme> Transference { get; set; } = Transferences.Transference.NothingToDo;

        public int ValencyPosition => PatternAttributesArrangement.PatternAttributes.ValencyPosition.GetValencyPosition(PatternAttributes);
    }
}
