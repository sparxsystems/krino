using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Vertical.Utils.Rules;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions.Rules
{
    /// <summary>
    /// Predefined inheritance rules.
    /// </summary>
    public static class MorphematicAdPositionRules
    {
        public static NothingRule<GrammarCharacter> Nothing => RuleMaker.Nothing<GrammarCharacter>();

        public static IsRule<GrammarCharacter> Epsilon => RuleMaker.Is(GrammarCharacter.e);

        public static IsRule<GrammarCharacter> U => RuleMaker.Is(GrammarCharacter.U);

        public static IsRule<GrammarCharacter> E => RuleMaker.Is(GrammarCharacter.E);

        public static OrRule<GrammarCharacter> Epsilon_U => Epsilon | U;

        public static OrRule<GrammarCharacter> Epsilon_U_E => Epsilon | U | E;
    }
}
