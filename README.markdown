# Ningle-Bells

Tiny helper functions for Ningle -- A couple of bells and whistles, if you will.

## What is it good for?

Removing some of the cruft of working with Ningle (accessing params, for example, as well as outputting as text, html, and json)

## Why?

Ninglex makes several assumptions (and that's how it's intended! -- it's targeted toward beginners and small projects that are helped by those assumptions). I wanted something with some of the most helpful bits of Ninglex, but that could be used easily for more advanced projects.

## Usage

Largely the same as Ningle, except you can replace things like

```common-lisp
(setf (ningle:route *app* "/greet/:name")
      #'(lambda (params)
          (format nil "Hello, ~A" (cdr (assoc :name params)))))
```

with

```common-lisp
(setf (ningle:route *app* "/greet/:name")
      #'(lambda (params)
	      (with-request-params params ((n :name))
		    (string-response
			  (format nil "Hello, ~A" n)))))
```

(the string-response isn't completely necessary, but is good practice)

That's basically it, except there's also `html-response`, `json-response`, and `get-param-value` (for retrieving individual params from the param argument), but it's definitely a lot more convienient than just using plain ningle I feel.

## Acknowledgements

Ningle-Bells is helper functions to Eitaro Fukamachi's [Ningle](https://github.com/fukamachi/ningle), and was forked from Flavio Egoavil's [Ninglex](https://github.com/defunkydrummer/ninglex)

I'm a big fan of all of Eitaro's work and recommend you check it out if you haven't, and I never would've even thought to do this if not for Ninglex, so thanks to both of them.

## License

Licensed under the MIT license.
