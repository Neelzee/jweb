import { chromium, Browser, Page, Locator } from "playwright";
import { expect } from "playwright/test";

type Callback<A> = (a: A) => () => void;

type EffFn<A> =
  (onSuccess: Callback<A>) => (onError: Callback<Error>) => () => void;

export const launchImpl: EffFn<Browser>
  = (onSuccess) => (onError) => () => {
  chromium.launch({ headless: true })
    .then(b => onSuccess(b)())
    .catch(e => onError(e)());
};

export const newPageImpl
  = (browser: Browser): EffFn<Page> => (onSuccess) => (onError) => () => {
  browser.newPage()
    .then(p => onSuccess(p)())
    .catch(e => onError(e)());
};

export const gotoImpl
  = (page: Page) =>
    (url: string): EffFn<void> => (onSuccess) => (onError) => () => {
  page.goto(url)
    .then(() => onSuccess(undefined as unknown as void)())
    .catch(e => onError(e)());
};

export const titleImpl
  = (page: Page): EffFn<string> => (onSuccess) => (onError) => () => {
  page.title()
    .then(t => onSuccess(t)())
    .catch(e => onError(e)());
};

export const closeImpl
  = (browser: Browser): EffFn<void> => (onSuccess) => (onError) => () => {
  browser.close()
    .then(() => onSuccess(undefined as unknown as void)())
    .catch(e => onError(e)());
};

export const assertTitleImpl
  = (page: Page) => (expected: string): EffFn<void> => (onSuccess) => (onError) => () => {
  expect(page).toHaveTitle(expected)
    .then(() => onSuccess(undefined as unknown as void)())
    .catch((e: Error) => onError(e)());
};

export const setDefaultTimeoutImpl
  = (page: Page) => (ms: number): () => void => () => {
  page.setDefaultTimeout(ms);
  page.setDefaultNavigationTimeout(ms);
};

export const locatorImpl
  = (page: Page) => (selector: string): Locator =>
  page.locator(selector);

export const clickImpl
  = (locator: Locator): EffFn<void> => (onSuccess) => (onError) => () => {
  locator.click()
    .then(() => onSuccess(undefined as unknown as void)())
    .catch((e: Error) => onError(e)());
};

export const fillImpl
  = (locator: Locator) => (text: string): EffFn<void> => (onSuccess) => (onError) => () => {
  locator.fill(text)
    .then(() => onSuccess(undefined as unknown as void)())
    .catch((e: Error) => onError(e)());
};

export const assertCheckedImpl
  = (locator: Locator): EffFn<void> => (onSuccess) => (onError) => () => {
  expect(locator).toBeChecked()
    .then(() => onSuccess(undefined as unknown as void)())
    .catch((e: Error) => onError(e)());
};
