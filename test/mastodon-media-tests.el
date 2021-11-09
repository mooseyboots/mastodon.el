(require 'el-mock)

(ert-deftest mastodon-media:get-avatar-rendering ()
  "Should return text with all expected properties."
  (with-mock
   (mock (image-type-available-p 'imagemagick) => t)
   (mock (create-image * (when (version< emacs-version "27.1") 'imagemagick) t :height 123) => :mock-image)

   (let* ((mastodon-media--avatar-height 123)
          (result (mastodon-media--get-avatar-rendering "http://example.org/img.png"))
          (result-no-properties (substring-no-properties result))
          (properties (text-properties-at 0 result)))
     (should (string= "  " result-no-properties))
     (should (string= "http://example.org/img.png" (plist-get properties 'media-url)))
     (should (eq 'needs-loading (plist-get properties 'media-state)))
     (should (eq 'avatar (plist-get properties 'media-type)))
     (should (eq :mock-image (plist-get properties 'display))))))

(ert-deftest mastodon-media:get-media-link-rendering ()
  "Should return text with all expected properties."
  (with-mock
   (mock (create-image * nil t) => :mock-image)

   (let* ((mastodon-media--preview-max-height 123)
          (result
           (mastodon-media--get-media-link-rendering "http://example.org/img.png"
                                                     "http://example.org/remote/img.png"))
          (result-no-properties (substring-no-properties result))
          (properties (text-properties-at 0 result)))
     (should (string= "[img] " result-no-properties))
     (should (string= "http://example.org/img.png" (plist-get properties 'media-url)))
     (should (eq 'needs-loading (plist-get properties 'media-state)))
     (should (eq 'media-link (plist-get properties 'media-type)))
     (should (eq :mock-image (plist-get properties 'display)))
     (should (eq 'highlight (plist-get properties 'mouse-face)))
     (should (eq 'image (plist-get properties 'mastodon-tab-stop)))
     (should (string= "http://example.org/remote/img.png" (plist-get properties 'image-url)))
     (should (eq mastodon-tl--shr-image-map-replacement (plist-get properties 'keymap)))
     (should (string= "RET/i: load full image (prefix: copy URL), +/-: zoom, r: rotate, o: save preview"
                 (plist-get properties 'help-echo))))))

(ert-deftest mastodon-media:load-image-from-url:avatar-with-imagemagic ()
  "Should make the right call to url-retrieve."
  (let ((url "http://example.org/image.png")
        (mastodon-media--avatar-height 123))
    (with-mock
     (mock (image-type-available-p 'imagemagick) => t)
     (mock (create-image
            *
            (when (version< emacs-version "27.1") 'imagemagick)
            t :height 123) => '(image foo))
     (mock (copy-marker 7) => :my-marker )
     (mock (url-retrieve
            url
            #'mastodon-media--process-image-response
            `(:my-marker (:height 123) 1 ,url))
           => :called-as-expected)

     (with-temp-buffer
       (insert (concat "Start:"
                       (mastodon-media--get-avatar-rendering "http://example.org/img.png")
                       ":rest"))

       (should (eq :called-as-expected (mastodon-media--load-image-from-url url 'avatar 7 1)))))))

(ert-deftest mastodon-media:load-image-from-url:avatar-without-imagemagic ()
  "Should make the right call to url-retrieve."
  (let ((url "http://example.org/image.png"))
    (with-mock
     (mock (image-type-available-p 'imagemagick) => nil)
     (mock (create-image * nil t) => '(image foo))
     (mock (copy-marker 7) => :my-marker )
     (mock (url-retrieve
            url
            #'mastodon-media--process-image-response
            `(:my-marker () 1 ,url))
           => :called-as-expected)

     (with-temp-buffer
       (insert (concat "Start:"
                       (mastodon-media--get-avatar-rendering "http://example.org/img.png")
                       ":rest"))

       (should (eq :called-as-expected (mastodon-media--load-image-from-url url 'avatar 7 1)))))))

(ert-deftest mastodon-media:load-image-from-url:media-link-with-imagemagic ()
  "Should make the right call to url-retrieve."
  (let ((url "http://example.org/image.png"))
    (with-mock
     (mock (image-type-available-p 'imagemagick) => t)
     (mock (create-image * nil t) => '(image foo))
     (mock (copy-marker 7) => :my-marker )
     (mock (url-retrieve
            "http://example.org/image.png"
            #'mastodon-media--process-image-response
            '(:my-marker (:max-height 321) 5 "http://example.org/image.png"))
           => :called-as-expected)
     (with-temp-buffer
       (insert (concat "Start:"
                       (mastodon-media--get-media-link-rendering url)
                       ":rest"))
       (let ((mastodon-media--preview-max-height 321))
         (should (eq :called-as-expected (mastodon-media--load-image-from-url url 'media-link 7 5))))))))

(ert-deftest mastodon-media:load-image-from-url:media-link-without-imagemagic ()
  "Should make the right call to url-retrieve."
  (let ((url "http://example.org/image.png"))
    (with-mock
     (mock (image-type-available-p 'imagemagick) => nil)
     (mock (create-image * nil t) => '(image foo))
     (mock (copy-marker 7) => :my-marker )
     (mock (url-retrieve
            "http://example.org/image.png"
            #'mastodon-media--process-image-response
            '(:my-marker () 5 "http://example.org/image.png"))
           => :called-as-expected)

     (with-temp-buffer
       (insert (concat "Start:"
                       (mastodon-media--get-avatar-rendering url)
                       ":rest"))
       (let ((mastodon-media--preview-max-height 321))
         (should (eq :called-as-expected (mastodon-media--load-image-from-url url 'media-link 7 5))))))))

(ert-deftest mastodon-media:load-image-from-url:url-fetching-fails ()
  "Should cope with failures in url-retrieve."
  (let ((url "http://example.org/image.png")
        (mastodon-media--avatar-height 123))
    (with-mock
     (mock (image-type-available-p 'imagemagick) => t)
     (mock (create-image
            *
            (when (version< emacs-version "27.1") 'imagemagick)
            t :height 123) => '(image foo))
     (stub url-retrieve => (error "url-retrieve failed"))

     (with-temp-buffer
       (insert (concat "Start:"
                       (mastodon-media--get-avatar-rendering "http://example.org/img.png")
                       ":rest"))

       (should (eq :loading-failed (mastodon-media--load-image-from-url url 'avatar 7 1)))
       ;; the media state was updated so we won't load this again: 
       (should (eq 'loading-failed (get-text-property 7 'media-state)))))))

(ert-deftest mastodon-media:process-image-response ()
  "Should process the HTTP response and adjust the source buffer."
  (with-temp-buffer
    (with-mock
     (let ((source-buffer (current-buffer))
           used-marker
           saved-marker)
       (insert "start:")
       (setq used-marker (copy-marker (point))
             saved-marker (copy-marker (point)))
       ;; Mock needed for the preliminary image created in
       ;; mastodon-media--get-avatar-rendering
       (stub create-image => :fake-image)
       (insert (mastodon-media--get-avatar-rendering
                "http://example.org/image.png.")
               ":end")
       (with-temp-buffer
         (insert "some irrelevant\n"
                 "http headers\n"
                 "which will be ignored\n\n"
                 "fake\nimage\ndata")
         (goto-char (point-min))

         (mock (create-image
                "fake\nimage\ndata"
                (when (version< emacs-version "27.1") 'imagemagick)
                t ':image :option) => :fake-image)

         (mastodon-media--process-image-response
          () used-marker '(:image :option) 1 "http://example.org/image.png")

         ;; the used marker has been unset:
         (should (null (marker-position used-marker)))
         ;; the media-state has been set to loaded and the image is being displayed
         (should (eq 'loaded (get-text-property saved-marker 'media-state source-buffer)))
         (should (eq ':fake-image (get-text-property saved-marker 'display source-buffer))))))))

(ert-deftest mastodon-media:inline-images ()
  "Should process all media in buffer."
  (with-mock
   ;; Stub needed for the test setup:
   (stub create-image => '(image ignored))

   (let (marker-media-link marker-media-link-bad-url marker-false-media marker-avatar)
     (with-temp-buffer
       (insert "Some text before\n")
       (setq marker-media-link (copy-marker (point)))
       (insert (mastodon-media--get-media-link-rendering "http://example.org/i.jpg")
               " some more text ")
       (setq marker-media-link-bad-url (copy-marker (point)))
       (insert (mastodon-media--get-media-link-rendering "/files/small/missing.png")
               " some more text ")
       (setq marker-false-media (copy-marker (point)))
       (insert
        ;; text that looks almost like an avatar but lacks the media-url property
        (propertize "this won't be processed"
                    'media-state 'needs-loading
                    'media-type 'avatar)
        "even more text ")
       (setq marker-avatar (copy-marker (point)))
       (insert (mastodon-media--get-avatar-rendering "http://example.org/avatar.png")
               " end of text")
       (goto-char (point-min))

       ;; stub for the actual test:
       (stub mastodon-media--load-image-from-url)
       (mastodon-media--inline-images (point-min) (point-max))

       (should (eq 'loading (get-text-property marker-media-link 'media-state)))
       (should (eq 'invalid-url (get-text-property marker-media-link-bad-url 'media-state)))
       (should (eq 'loading (get-text-property marker-avatar 'media-state)))
       (should (eq 'needs-loading (get-text-property marker-false-media 'media-state)))))))
