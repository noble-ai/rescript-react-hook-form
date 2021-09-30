module Promise = {
  let map = (p, fn) => Js.Promise.then_(a => Js.Promise.resolve(fn(a)), p)
}

type typeopt = [
  | #required
  | #maxLength
  | #minLength
  | #max
  | #min
  | #validate
]

type opt<'t> = {
  value: 't,
  message: string,
}

// This validation result is string or bool so in rescript make it opaque - AxM
type validation
type opts<'t> = {
  required: option<string>,
  maxLength: option<opt<int>>,
  minLength: option<opt<int>>,
  max: option<opt<float>>,
  min: option<opt<float>>,
  validate: option<'t => Js.Promise.t<validation>>,
}

// ReactHookForm is polymorphic in options types so we cast to this opaque type to pass it thorugh - AxM
type optsImpl
external optsToImpl: opts<'a> => optsImpl = "%identity"

// validate functions are expected to return either a message string or boolean and we dont have a sum type like that
// so instead just sneak this boolean true through as string - AxM
// TODO: this could be done with rescript @unwrap? -AxM
let validateTrue: validation = %raw(`true`)
external validateMessage: string => validation = "%identity"

let multiplexValidate = (validate, value) => {
  validate(value)
  ->Promise.map((res) => 
    switch res {
    | Some(message) => validateMessage(message)
    | None => validateTrue
    }
   )
}

let makeOptsString = (
  ~required: option<string>=?,
  ~maxLength: option<opt<int>>=?,
  ~minLength: option<opt<int>>=?,
  ~validate: option<string => Js.Promise.t<option<string>>>=?,
  (),
): optsImpl =>
  {
    validate: validate->Belt.Option.map(multiplexValidate),
    required: required,
    minLength: minLength,
    maxLength: maxLength,
    min: None,
    max: None,
  }->optsToImpl

let makeOptsFloat = (
  ~required: option<string>=?,
  ~max: option<opt<float>>=?,
  ~min: option<opt<float>>=?,
  ~validate: option<float => Js.Promise.t<option<string>>>=?,
  (),
): optsImpl =>
  {
    validate: validate->Belt.Option.map(multiplexValidate),
    required: required,
    minLength: None,
    maxLength: None,
    min: min,
    max: max,
  }->optsToImpl

let makeOptsInt = (
  ~required: option<string>=?,
  ~max: option<opt<float>>=?,
  ~min: option<opt<float>>=?,
  ~validate: option<int => Js.Promise.t<option<string>>>=?,
  (),
): optsImpl =>
  {
    validate: validate->Belt.Option.map(multiplexValidate),
    required: required,
    minLength: None,
    maxLength: None,
    min: min,
    max: max,
  }->optsToImpl


// registration function to be passed directly to ref= or take options.
// so lets leave it opaque and provide magic translation both ways - AxM
type register
type error = {
  \"type": typeopt, //Error Type. eg: required, min, max, minLength
  // // types: Js.Dict.t<string, 	Record<{ string, string | boolean }>	
  message: string, // | React.ReactElement	
  // ref	React.RefObject	Reference for your input element.
}

type errors = Js.Dict.t<error>

type formstate = {
  isDirty: bool,
  // dirtyFields: 	object	
  isSubmitted: bool,
  isSubmitSuccessful: bool,
  isSubmitting: bool,
  submitCount: int,
  isValid: bool,
  isValidating: bool,
  errors: errors,
}

type watch
let toWatch1: (watch) => (. string, string) => string = %raw(` (i) => i `)
external toWatchAll: (watch) => (. () ) => Js.t<'a> = "%identity"

type getvalues
let toGetValues1: (getvalues) => (. string, string) => string = %raw(` (i) => i `)
external toGetValuesAll: (getvalues) => (. () ) => Js.t<'a> = "%identity"

type handleSubmit<'data, 'err> = (
  (. Js.t<'data>, ReactEvent.Form.t) => Js.Promise.t<unit>,
  ReactEvent.Form.t,
) => unit

type mode = [ #onChange | #onBlur | #onSubmit | #onTouched ] // | all = 'onSubmit'
type reValidateMode = [ #onChange | #onBlur | #onSubmit ] 
type optsUseForm = 
{
  mode: option<mode>,
  reValidateMode: option<reValidateMode>,
  // defaultValues: {},
  // resolver: undefined,
  // context: undefined,
  // criteriaMode: "firstError",
  // shouldFocusError: true,
  // shouldUnregister: true,
}

let makeOptsUseForm = (~mode: option<mode>=?, ~reValidateMode: option<reValidateMode>=?, ()): option<optsUseForm> => {
  mode,
  reValidateMode
}
->Some

type useformImpl<'data, 'err> = {
  register: register,
  handleSubmit: handleSubmit<'data, 'err>,
  watch: watch,
  setValue: (. string, string) => unit,
  getValues: getvalues,
  errors: errors,
  trigger: unit => Js.Promise.t<unit>,
  formState: formstate,
}

@module("react-hook-form")
external useFormImpl: (option<optsUseForm>) => useformImpl<'data, 'err> = "useForm"

external registerToRef: register => ReactDOM.Ref.callbackDomRef = "%identity"
let registerOptsToRef: (
  register,
  optsImpl,
) => ReactDOM.Ref.callbackDomRef = %raw(` (register, opts) => register(opts) `)
let registerToCustom: (register) => (. string) => ReactDOM.Ref.callbackDomRef = %raw(` (i) => i `)

type useform<'ref, 'data, 'err, 'all> = {
  // Controller needs the raw form so return it along with our convenient typed accessors - AxM
  impl: useformImpl<'data, 'err>,
  register: ReactDOM.domRef,
  registerOpts: (~opts: optsImpl) => ReactDOM.domRef,
  registerCustom: (string) => ReactDOM.domRef,
  handleSubmit: handleSubmit<'data, 'err>,
  watch: (string, string) => string,
  watchAll: unit => Js.t<'all>,
  getValues: (string, string) => string,
  getValuesAll: unit => Js.t<'all>,
  errors: errors,
  trigger: unit => Js.Promise.t<unit>,
  formState: formstate,
  setValue: (. string, string) => unit,
}

let useForm = (~mode: option<mode>=?, ()) => {
  let form = useFormImpl(makeOptsUseForm(~mode?, ()))
  let registerOpts = (~opts) => {
    form.register
    ->registerOptsToRef(opts)
    ->ReactDOM.Ref.callbackDomRef
  }

  let registerCustom = (str) => {
    registerToCustom(form.register)(.str)
    ->ReactDOM.Ref.callbackDomRef
  }

  let register = form.register->registerToRef->ReactDOM.Ref.callbackDomRef

  let watch1 = (a, b) => {
    toWatch1(form.watch)(. a, b)
  }
  let watchAll = () => {
    toWatchAll(form.watch)(. )
  }
  let getValues1 = (a, b) => {
    toGetValues1(form.getValues)(. a, b)
  }
  let getValuesAll = () => {
    toGetValuesAll(form.getValues)(. )
  }


  {
    impl: form,
    register,
    registerOpts,
    registerCustom,
    handleSubmit: form.handleSubmit,
    watch: watch1,
    watchAll,
    setValue: form.setValue,
    getValues: getValues1,
    getValuesAll,
    errors: form.errors,
    trigger: form.trigger,
    formState: form.formState,
  }
}

let hasError = (errors: errors, field) => errors->Js.Dict.get(field)->Belt.Option.isSome

module Controller = {
  type props<'value> = {
   onChange: (ReactEvent.Form.t) => unit,
   onBlur: () => unit,
   value: 'value,
   name: string,
   ref: ReactDOM.domRef
  }
  type state = { invalid: bool, isTouched: bool, isDirty: bool }

  @module("react-hook-form")
  @react.component
  external make: (
    ~name: string,
    ~control: useformImpl<'data, 'err>,
    ~render: ( props<'value>, state) => React.element,
    ~defaultValue: 'value,
    ~rules: option<optsImpl>=?,
    ~onFocus: option<() => unit>=?,
  ) => React.element = "Controller"
}
