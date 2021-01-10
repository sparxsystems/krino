using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions.Rules.Strings;
using Krino.Vertical.Utils.Rules;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions.Rules
{
    /// <summary>
    /// Predefined morph rules.
    /// </summary>
    public static class MorphRules
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
        /// Accepts the empty string morph.
        /// </summary>
        public static IRule<string> EmptyString => Is("");

        /// <summary>
        /// All morphs which are not null or empty string are accepted.
        /// </summary>
        public static IRule<string> Something => RuleMaker.IsNotNull<string>().And(RuleMaker.Is("").Not());

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
        /// Only words ending with the consonant are accepted.
        /// </summary>
        public static IRule<string> EndsWithConsonant => new EndsWithConsonantRule();
    }
}
