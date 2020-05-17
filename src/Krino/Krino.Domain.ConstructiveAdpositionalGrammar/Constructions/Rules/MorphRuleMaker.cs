using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules.Strings;
using Krino.Vertical.Utils.Rules;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules
{
    /// <summary>
    /// Utility to create morph rules.
    /// </summary>
    public static class MorphRuleMaker
    {
        /// <summary>
        /// It does not accept any morph.
        /// </summary>
        public static IRule<string> Nothing => RuleMaker.Nothing<string>();

        /// <summary>
        /// All morphs are accepted.
        /// </summary>
        public static IRule<string> Anything => RuleMaker.Anything<string>();

        /// <summary>
        /// Only empty string morph is accepted.
        /// </summary>
        public static IRule<string> EmptyString => RuleMaker.Is("");

        /// <summary>
        /// Anything except the empty string morph is accepted.
        /// </summary>
        public static IRule<string> NotEmptyString => RuleMaker.IsNot("");

        /// <summary>
        /// Morph with the specified value is accepted.
        /// </summary>
        /// <param name="value"></param>
        /// <returns></returns>
        public static IRule<string> Is(string value) => RuleMaker.Is(value);

        /// <summary>
        /// Only words having the specified suffix are accepted.
        /// </summary>
        /// <param name="suffix"></param>
        /// <returns></returns>
        public static IRule<string> Suffix(string suffix) => new SuffixRule(suffix);

        /// <summary>
        /// Only words anding with the consonant are accepted.
        /// </summary>
        public static IRule<string> EndsWithConsonant => new EndsWithConsonantRule();
    }
}
