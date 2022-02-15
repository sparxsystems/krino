using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes.Sememes
{
    /// <summary>
    /// Semantic attributes of verbs.
    /// </summary>
    public class VerbSememes : EnumGroupBase
    {
        public VerbSememes(EnumGroupBase parent) : base(parent)
        {
            Person = new PersonSememes(this);
            Number = new NumberSememes(this);
            Time = new VerbTimeSememes(this);
            Aspect = new VerbAspectSememes(this);
            Mood = new VerbMoodSememes(this);
            Voice = new VerbVoiceSememes(this);
        }

        public PersonSememes Person { get; }

        /// <summary>
        /// Singular or plural.
        /// </summary>
        public NumberSememes Number { get; }

        /// <summary>
        /// Refers to the time (the perception of the reality): past, present and future.
        /// </summary>
        /// <remarks>
        /// Note: Tense is a grammatical category combining Time and Aspect.
        /// </remarks>
        public VerbTimeSememes Time { get; }

        /// <summary>
        /// Refers to the flow of time.
        /// </summary>
        /// <remarks>
        /// Note: Tense is a grammatical category combining Time and Aspect.
        /// </remarks>
        public VerbAspectSememes Aspect { get; }

        /// <summary>
        /// Refers to the attitude of the action.
        /// </summary>
        public VerbMoodSememes Mood { get; }

        /// <summary>
        /// Refers who does the verb (active or passive).
        /// </summary>
        public VerbVoiceSememes Voice { get; }
    }
}
